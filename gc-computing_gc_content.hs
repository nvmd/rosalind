-- http://rosalind.info/problems/gc/
import Data.List (intersperse)
type DnaString = String
data FastaString = FastaString String DnaString

instance Show FastaString where
  show (FastaString id dna) = ">" ++ id ++ "\n" ++ dna
--  showList (x:[]) = (\s -> (show x) ++ s)
--  showList (x:xs) = (\s -> (show x) ++ "\n" ++ show xs ++ s)
  showList xs = (\s -> concat (intersperse "\n" $ map show xs) ++ s)
  
readFasta s = [ FastaString
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
highestGcContent (x:xs) = foldl compareGcContent
                          (fastaId x, fastaGcContent x)
                          xs
                          where
                            compareGcContent (id, content) e = if newContent > content
                              then (fastaId e, newContent)
                              else (id, content)
                              where newContent = fastaGcContent e

main :: IO ()
main = do
  rawFasta <- getContents
  let fasta = readFasta rawFasta
  
  let (highestId, content) = highestGcContent fasta
  putStrLn $ highestId ++ "\n" ++ (show content)

