{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Language.MI.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Parsec
import Text.Parsec.String
import Control.Applicative hiding ((<|>), many)
import Language.Haskell.Meta.Parse (parseDecs)
import Data.List.Split hiding (oneOf)
import Data.List

import Data.IORef
import System.IO.Unsafe

dicTable = unsafePerformIO (newIORef [("dicName", ["FuncName"])])

mi = QuasiQuoter 
    { quoteExp  = undefined
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = multipleInstance
    }

pMultipleInstance :: Parser (String, String, String)
pMultipleInstance = do
    spaces >> string "instance"
    dic <- (many1 space *> pDicName)
    spaces >> string "::"
    typeclass <- (spaces *> pTypeClassName)
    datatype <- (many1 space *> pDataTypeName)
    (many1 space) >> string "where"
    funcdef <- (many1 space *> pFuncDef)
    return $ (,,) dic typeclass funcdef

pDicName = (:) <$> lower <*> many1 alphaNum
pTypeClassName = (:) <$> upper <*> many1 alphaNum
pDataTypeName = (:) <$> upper <*> many1 alphaNum
pFuncDef = many1 anyToken

pFunc :: Parser String
pFunc = do
    spaces >> string "mi" >> spaces >> string "=" >> spaces
    dic <- ((:) <$> lower <*> many alphaNum)
    funcdef <- many anyToken
    return dic

addUnderbar :: String -> String
addUnderbar x = x ++ "_"

sub :: String -> Parser String
sub x = do
    s <- oneOf ">(,|-` ="
    string x
    t <- oneOf "),|` "
    return $ s : (addUnderbar(x) ++ [t])

replaceMatchAll s line = 
  case parse replaceAllParser "" line of
    Left _ -> line
    Right x -> x
  where
    replace1Parser =
      do
        n <- manyTill anyChar (lookAhead $ try s)
        m <- s
        return (n ++ m)
    replaceAllParser =
      do
        n <- replace1Parser 
        m <- try replaceAllParser <|> many anyChar
        return (n ++ m)

multipleInstance input = do
  case parse pFunc "Func Parser" input of
    (Right dic) -> miFunc dic input
    (Left _) -> case parseDecs input of
                  (Right x) -> case x of
                                 ((ClassD cxt' name tv _ funcSig):rest) -> miClass cxt' name tv funcSig
                                 _ -> error "unexpected"
                  (Left _) -> case parse pMultipleInstance "Instance Parser" input of
                                (Right (dic, typeclass, funcdef)) -> miInstance dic typeclass funcdef
                                (Left _) -> error "unexpected"
                      
miClass c name tv funcSig = do
    return $ [DataD c
             (mkName $ nameBase $ name)
             tv
             [RecC (mkName $ nameBase $ name) (funcDefSig funcSig)]
             []]
             where
               funcDefSig = foldr (\(SigD n t) a -> (n, NotStrict, t) : a) []

miInstance dic typeclass funcdef = do
    runIO $ do table <- readIORef dicTable
               writeIORef dicTable (table ++ [(dic, getFuncName $ toFuncD input)])
    return $ [ValD (VarP $ mkName dic)
             (NormalB (RecConE (mkName typeclass) (body src)))
             (subfunc src)
             ]
             where
               input = splitOn "\n" funcdef
               src = mkNewFunc $ toFuncD input
               toFuncD = concat . foldr (\str a -> case parseDecs str of
                                            (Left err) -> error err
                                            (Right x) -> x : a) []
               mkNewFunc = foldr (\(FunD name def) a -> ((mkName $ nameBase name), def, (mkName $ (nameBase name) ++ "'")) : a) []
               getFuncName = foldr (\(FunD name def) a -> (nameBase name): a) []
               body = foldr (\(name, def, name') a -> (name, VarE name') : a) []
               subfunc = foldr (\(name, def, name') a -> (FunD name' def) : a) []

miFunc dic funcdef = do
    funcs <- runIO $ do table <- readIORef dicTable
                        case lookup dic table of
                          Nothing -> return []
                          Just v -> return v
    orgDef <- return $ extractDef $ toFuncD funcdef
    fn <- return $ nameBase $ getFuncName orgDef
    newDef <- return $ trans funcdef $ fn:funcs
    funcds <- return $ filter (not . isSigD) $ extractDef $ toFuncD newDef
    body <- return $ toFuncD $ mkDefFunc fn dic funcds
    defaultFunc <- return $ case find isSigD orgDef of
                              Just s -> s : body
                              Nothing -> body
    return $ (mkExplicitFunc (changeName (mkName fn) funcds) funcs) ++ defaultFunc
    where
      isSigD a = case a of
                   (SigD _ _ ) -> True
                   otherwise -> False
      toFuncD s = case parseDecs s of
                   Left err -> error err
                   Right x -> x
      trans = foldr (\d a -> replaceMatchAll (sub d) a)
      extractDef ((ValD _ _ d):_) = d
      
      getFuncName ((FunD name _):_) = name
      getFuncName (_:xs) = getFuncName xs

      addDash s = s ++ "'"

      mkExplicitFunc ((FunD name c):fs) dic = (FunD (mkName $ addDash (nameBase name)) (clausepat dic (nameBase name) c)):fs

      clausepat dic fn = foldr (\(Clause p b d) a -> (Clause ([VarP $ mkName "dic"] ++ p) b (d ++ (dicapp dic) ++ (recursiveFunc fn))) : a) []
      dicapp [] = []
      dicapp (x:xs) = (ValD (VarP $ mkName $ addUnderbar x) (NormalB (AppE (VarE $ mkName x) (VarE $ mkName "dic"))) []) : (dicapp xs)
      recursiveFunc fn = [(ValD (VarP $ mkName $ addUnderbar fn) (NormalB (AppE (VarE $ mkName $ addDash fn) (VarE $ mkName "dic"))) [])]

      changeName fn = foldr (\(FunD name c) a -> (FunD fn c) : a) []
      mkArgs ((FunD _ ((Clause args _ _):_)):_) = foldr (\x y -> x ++ " " ++ y) [] (take (length args) $ splitOn "," $ intersperse ',' ['a'..'z'])
      mkDefFunc fn d a = foldr (++) "" [fn, " ", args, " = ", addDash fn, " ", d, " ", args]
                        where
                          args = mkArgs a
