module Computer where

import Data.Bits

data Register = Zero
              | One
              | Two
              | Three
              | Four
              | Five
              deriving (Show, Eq)

data Reference = Immediate Int
               | Register  Register
              deriving (Show, Eq)

data Instruction = Add Register  Reference Register
                 | Mul Register  Reference Register
                 | Ban Register  Reference Register
                 | Bor Register  Reference Register
                 | Set Reference Register
                 | Gt  Reference Reference Register
                 | Eq  Reference Reference Register
              deriving (Show, Eq)

newtype Program = Program [Instruction]
              deriving (Show, Eq)

newtype Memory = Memory (Int, Int, Int, Int, Int, Int)
              deriving (Show, Eq)
zeroed :: Memory
zeroed = Memory (0,0,0,0,0,0)

set :: Register -> Int -> State -> State
set Zero  v (State r ip (Memory (_,b,c,d,e,f))) = State r ip (Memory (v,b,c,d,e,f))
set One   v (State r ip (Memory (a,_,c,d,e,f))) = State r ip (Memory (a,v,c,d,e,f))
set Two   v (State r ip (Memory (a,b,_,d,e,f))) = State r ip (Memory (a,b,v,d,e,f))
set Three v (State r ip (Memory (a,b,c,_,e,f))) = State r ip (Memory (a,b,c,v,e,f))
set Four  v (State r ip (Memory (a,b,c,d,_,f))) = State r ip (Memory (a,b,c,d,v,f))
set Five  v (State r ip (Memory (a,b,c,d,e,_))) = State r ip (Memory (a,b,c,d,e,v))

get :: Reference -> State -> Int
get (Immediate v   ) _                                  = v
get (Register Zero ) (State _ _ (Memory (v,_,_,_,_,_))) = v
get (Register One  ) (State _ _ (Memory (_,v,_,_,_,_))) = v
get (Register Two  ) (State _ _ (Memory (_,_,v,_,_,_))) = v
get (Register Three) (State _ _ (Memory (_,_,_,v,_,_))) = v
get (Register Four ) (State _ _ (Memory (_,_,_,_,v,_))) = v
get (Register Five ) (State _ _ (Memory (_,_,_,_,_,v))) = v

data State = State Register Int Memory
              deriving (Show, Eq)

initialize :: Int -> (Register, [Instruction]) -> (Program, State)
initialize a (r, program) = (Program program, set Zero a $ State r 0 zeroed)

ipToMem :: State -> State
ipToMem (State Zero  ip (Memory (_,b,c,d,e,f))) = State Zero  ip (Memory (ip,b,c,d,e,f))
ipToMem (State One   ip (Memory (a,_,c,d,e,f))) = State One   ip (Memory (a,ip,c,d,e,f))
ipToMem (State Two   ip (Memory (a,b,_,d,e,f))) = State Two   ip (Memory (a,b,ip,d,e,f))
ipToMem (State Three ip (Memory (a,b,c,_,e,f))) = State Three ip (Memory (a,b,c,ip,e,f))
ipToMem (State Four  ip (Memory (a,b,c,d,_,f))) = State Four  ip (Memory (a,b,c,d,ip,f))
ipToMem (State Five  ip (Memory (a,b,c,d,e,_))) = State Five  ip (Memory (a,b,c,d,e,ip))

memToIp :: State -> State
memToIp (State Zero  _ (Memory (a,b,c,d,e,f))) = State Zero  a (Memory (a,b,c,d,e,f))
memToIp (State One   _ (Memory (a,b,c,d,e,f))) = State One   b (Memory (a,b,c,d,e,f))
memToIp (State Two   _ (Memory (a,b,c,d,e,f))) = State Two   c (Memory (a,b,c,d,e,f))
memToIp (State Three _ (Memory (a,b,c,d,e,f))) = State Three d (Memory (a,b,c,d,e,f))
memToIp (State Four  _ (Memory (a,b,c,d,e,f))) = State Four  e (Memory (a,b,c,d,e,f))
memToIp (State Five  _ (Memory (a,b,c,d,e,f))) = State Five  f (Memory (a,b,c,d,e,f))

incIp :: State -> State
incIp (State r ip mem) = State r (ip+1) mem

apply :: State -> Instruction -> State
apply s (Add a b c) = set c (get (Register a) s  +  get b s) s
apply s (Mul a b c) = set c (get (Register a) s  *  get b s) s
apply s (Ban a b c) = set c (get (Register a) s .&. get b s) s
apply s (Bor a b c) = set c (get (Register a) s .|. get b s) s
apply s (Set a c)   = set c (get a s) s
apply s (Gt a b c)  = set c (if get a s > get b s then 1 else 0) s
apply s (Eq a b c)  = set c (if get a s == get b s then 1 else 0) s

current :: Program -> State -> Instruction
current (Program p) (State _ ip _) = p !! ip

terminated :: Program -> State -> Bool
terminated (Program instructions) (State _ ip _) = ip < 0 || length instructions <= ip

runUntil :: (Program -> State -> Bool) -> Program -> State -> State
runUntil done p s = 
   if done p s
   then s
   else runUntil done p $ run' s
   where
      run' :: State -> State
      run' = incIp . memToIp . applyCurrent . ipToMem  -- . show'
      applyCurrent :: State -> State
      applyCurrent s' = apply s' (current p s')

run :: Program -> State -> Int
run p s =
   let s' = runUntil terminated p s
   in  get (Register Zero) s'
