-- http://rosalind.info/problems/gc/
import Data.List (intersperse)
--import Control.Monad (liftM)
import Text.ParserCombinators.Parsec

type DnaString = String
data FastaString = FastaString String DnaString

instance Show FastaString where
  show (FastaString id dna) = ">" ++ id ++ "\n" ++ dna
--  showList (x:[]) = (\s -> (show x) ++ s)
--  showList (x:xs) = (\s -> (show x) ++ "\n" ++ show xs ++ s)
  showList xs = (\s -> concat (intersperse "\n" $ map show xs) ++ s)

parseFasta :: String -> [FastaString]
parseFasta s = case (parse (parseFastaEntry `sepEndBy` newline) "fasta_source" s) of
                 Left err -> error $ "Can't read FASTA: " ++ show err
                 Right xs -> xs

parseFastaEntry = do
  name <- parseName
  newline
  dna <- parseDna
  return (FastaString name dna)

parseName = do
  char '>'
  name <- many1 $ alphaNum <|> char '_'
  return name

parseDna = do
--  dna <- liftM concat $ (many1 $ oneOf "ACGTacgt") `sepBy` newline
  dna <- many1 (oneOf "ACGTacgt")
  return dna

-- Test data
n3 = ">Rosalind_6404\nCCTGCGGAAGATCGGCACTAGAATAGCCA\nGAACCGTTTCTCTGAGGCTTCCGGCCTTCCCTCCCACTAATAATTCTGAGG\n>Rosalind_5959\nCCATCGGTAGCGCATCCTTAG\nTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCTATATCCATTTGTCAGCAGACACGC\n>Rosalind_0808\nCCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCC\nGGACTGGGAACCTGCGGGCAGTAGGTGGAAT"
n = ">Rosalind_6404\nCCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCCTCCCACTAATAATTCTGAGG\n>Rosalind_5959\nCCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCTATATCCATTTGTCAGCAGACACGC\n>Rosalind_0808\nCCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCC\nGGACTGGGAACCTGCGGGCAGTAGGTGGAAT"
n2 = ">Rosalind_6404\nCCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCCTCCCACTAATAATTCTGAGG\n>Rosalind_5959\nCCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCTATATCCATTTGTCAGCAGACACGC\n>Rosalind_0808\nCCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGACTGGGAACCTGCGGGCAGTAGGTGGAAT\n"

nn2 = parseFasta n2
nn3 = parseFasta n3
nn = parseFasta n
t2 = parseFasta . show
tt = t2 t
t = readFastaStub ""

readFastaStub s = [ FastaString
                    "Rosalind_6404"
                    "CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCCT\
                    \CCCACTAATAATTCTGAGG"
                  , FastaString
                    "Rosalind_5959"
                    "CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCTA\
                    \TATCCATTTGTCAGCAGACACGC"
                  , FastaString
                    "Rosalind_0808"
                    "CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGACT\
                    \GGGAACCTGCGGGCAGTAGGTGGAAT"
                  ]
-- End of test data

readFasta = parseFasta
--readFasta = readFastaStub
           
fastaId :: FastaString -> String
fastaId (FastaString id _) = id

fastaGcContent :: FastaString -> Float
fastaGcContent (FastaString _ dna) = gcContent dna

gcContent :: DnaString -> Float
gcContent dna = (\(gc, len) -> gc / len * 100)
              $ foldl count (0, 0) dna
                where 
                  count (gc, len) 'G' = (gc + 1, len + 1)
                  count (gc, len) 'C' = (gc + 1, len + 1)
                  count (gc, len)  _  = (gc,     len + 1)

highestGcContent :: [FastaString] -> (String, Float)
highestGcContent (x:[]) = (fastaId x, fastaGcContent x)
highestGcContent (x:xs) = foldl compareGcContent
                          (fastaId x, fastaGcContent x)
                          xs
                          where
                            compareGcContent (id, content) e = if newContent > content
                              then (fastaId e, newContent)
                              else (id, content)
                              where newContent = fastaGcContent e
highestGcContent []     = ("", 0)

main :: IO ()
main = do
  rawFasta <- getContents
  let fasta = readFasta rawFasta
  
  let (highestId, content) = highestGcContent fasta
  putStrLn $ highestId ++ "\n" ++ (show content)

