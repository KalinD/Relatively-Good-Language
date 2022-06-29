module MyCodeGen (codeGen) where

import Sprockell

codeGen :: Integer -> [Instruction]
codeGen n = [ 
         Load (ImmValue $ fromInteger n) regE  -- upper bound is hardcoded
       , Load (ImmValue 0) regA                -- first number
       , Load (ImmValue 1) regB                -- second number

       -- "beginloop"
       , Compute Gt regA regE regC             -- regA > regE ?
       , Branch regC (Abs 12)                  -- then jump to target "end"
       , WriteInstr regA numberIO              -- output regA
       , Compute Add regA regB regA
       , Compute Gt regB regE regC             -- regB > regE
       , Branch regC (Abs 12)                  -- target "end"
       , WriteInstr regB numberIO              -- output regB
       , Compute Add regA regB regB
       , Jump (Rel (-8))                       -- target "beginloop"

       -- "end"
       , EndProg
       ]

fibGen :: Integer -> [Instruction]
fibGen n = [
          Load (ImmValue $ fromInteger n) regA,     -- n = (value from the argument)
          Load (ImmValue 0) regB,                   -- i = 0
          Load (ImmValue 0) regC,                   -- fst = 0
          Load (ImmValue 1) regD,                   -- snd = 1
          Compute Gt regA regB regE,                -- n > i
          Branch regC (Rel (2)),                    -- go into the while loop  
          Jump (Abs 12),                            -- skip the while loop
          Load (IndAddr regD) regF,                 -- temp = snd
          Compute Add regC regD regD,               -- snd = fst + snd
          Load (IndAddr regF) regC,                 -- fst = temp
          Compute Incr regB regB regB,              -- i = i + 1
          Jump (Rel (-7)),                          -- back to the start of the while loop
          WriteInstr regD numberIO,                 -- ouput snd

          -- end
          EndProg
          ]

fprog :: [Instruction]
fprog = [ ReadInstr numberIO            -- ask the user for a number
       , Receive regE                  -- save the number in regE

       , Load (ImmValue 0) regA        -- first number
       , Load (ImmValue 1) regB        -- second number

       -- "beginloop"
       , Compute Gt regA regE regC     -- regA > regE ?
       , Branch regC (Abs 13)          -- then jump to target "end"
       , WriteInstr regA numberIO      -- output regA
       , Compute Add regA regB regA
       , Compute Gt regB regE regC     -- regB > regE
       , Branch regC (Abs 13)          -- target "end"
       , WriteInstr regB numberIO      -- output regB
       , Compute Add regA regB regB
       , Jump (Rel (-8))               -- target "beginloop"

       -- "end"
       , EndProg
       ]

go = run [fprog, fprog, fprog]
