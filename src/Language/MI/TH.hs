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

appendUnderbar :: String -> String
appendUnderbar x = x ++ "_"

sub :: String -> Parser String
sub x = do
    s <- oneOf ">(,|-` ="
    string x
    t <- oneOf "),|` "
    return $ s : (appendUnderbar(x) ++ [t])

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
    fn <- return $ nameBase $ funcname $ tof $ toFuncD funcdef
    --runIO $ print fn
    changed <- return $ trans funcdef (fn:funcs)
    --runIO $ print changed
    funcds <- return $ tof $ toFuncD changed
    --runIO $ print funcds
    t <- return $ mkExplicitFunc (namechange (mkName fn) funcds) funcs
    --runIO $ print t
    imp <- return $ toFuncD $ mkimpfunc fn dic funcds
    --runIO $ print imp
    return $ (mkExplicitFunc (namechange (mkName fn) funcds) funcs) ++ imp
    where
      flapflap = (InfixE (Just (VarE 'flip)) (VarE $ mkName ".") (Just (InfixE (Just (VarE 'flip)) (VarE $ mkName ".") Nothing)))
      valFunc dic ((FunD name _):fs) = ValD (VarP $ mkName $ nameBase name) (NormalB (AppE (AppE flapflap (VarE $ mkName $ (nameBase name) ++ "'")) (VarE $ mkName dic))) []
      mkExplicitFunc ((FunD name c):fs) dic = (FunD (mkName $ (nameBase name) ++ "'") (clausepat dic (nameBase name) c)):fs
      toFuncD s = case parseDecs s of
                   Left err -> error err
                   Right x -> x
      trans = foldr (\d a -> replaceMatchAll (sub d) a)
      tof ((ValD p b d):xs) = d
      funcname ((FunD name c):xs) = name
      clausepat dic fn = foldr (\(Clause p b d) a -> (Clause ([VarP $ mkName "dic"]++p) b (d++(dicapp dic)++(recursivefunc fn))) : a) []
      dicapp [] = []
      dicapp (x:xs) = (ValD (VarP $ mkName $ x++"_") (NormalB (AppE (VarE $ mkName x) (VarE $ mkName "dic"))) []) : (dicapp xs)
      recursivefunc fn = [(ValD (VarP $ mkName $ fn++"_") (NormalB (AppE (VarE $ mkName $ fn++"'") (VarE $ mkName "dic"))) [])]
      namechange fn = foldr (\(FunD name c) a -> (FunD fn c) : a) []
      ttt ((FunD name ((Clause p b d):cs)):fs) = foldr (\x y -> (x++" ")++y) [] (take (length p) $ splitOn "," $ intersperse ',' ['a'..'z'])
      mkimpfunc fn d a = fn++" "++(ttt a)++" = "++fn++"'"++" "++d++" "++(ttt a)
