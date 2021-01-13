module Computer where

import Data.Bits
import Text.Printf

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

class State a where
   memory :: a ->  Memory
   register :: a -> Register
   ip :: a -> Int

   withMemory :: Memory -> a -> a
   withIp :: Int -> a -> a

   set :: Register -> Int -> a -> a
   set r v s = withMemory (set' r (Immediate v) $ memory s) s
   get :: Reference -> a -> Int
   get r = get' r . memory

data Instruction = Add Register  Reference Register
                 | Mul Register  Reference Register
                 | Div Register  Reference Register
                 | Ban Register  Reference Register
                 | Bor Register  Reference Register
                 | Set Reference Register
                 | Gt  Reference Reference Register
                 | Eq  Reference Reference Register
                 | Noop -- this exists only as padding when simplifying the input
            deriving (Show, Eq)

data Step a = Op Instruction
            | Custom (a -> a)

newtype Program a = Program [Step a]

instance State s => Show (Program s) where
   show (Program [])            = ""
   show (Program (Op i:is))     = show i ++ "\n" ++ show (Program is)
   show (Program (Custom _:is)) = "(custom function)\n" ++ show (Program is)


newtype Memory = Memory (Int, Int, Int, Int, Int, Int)
              deriving (Eq)

instance Show Memory where
   show (Memory (a,b,c,d,e,f))= printf "%3d %6d %6d %10d %10d %14d" a b c d e f

zeroed :: Memory
zeroed = Memory (0,0,0,0,0,0)

set' :: Register -> Reference -> Memory -> Memory
set' a       (Register c)   mem                    = set' a (Immediate $ get' (Register c) mem) mem
set' Zero    (Immediate v)  (Memory (_,b,c,d,e,f)) = Memory (v,b,c,d,e,f)
set' One     (Immediate v)  (Memory (a,_,c,d,e,f)) = Memory (a,v,c,d,e,f)
set' Two     (Immediate v)  (Memory (a,b,_,d,e,f)) = Memory (a,b,v,d,e,f)
set' Three   (Immediate v)  (Memory (a,b,c,_,e,f)) = Memory (a,b,c,v,e,f)
set' Four    (Immediate v)  (Memory (a,b,c,d,_,f)) = Memory (a,b,c,d,v,f)
set' Five    (Immediate v)  (Memory (a,b,c,d,e,_)) = Memory (a,b,c,d,e,v)

get' :: Reference -> Memory -> Int
get' (Immediate v   ) _                      = v
get' (Register Zero ) (Memory (v,_,_,_,_,_)) = v
get' (Register One  ) (Memory (_,v,_,_,_,_)) = v
get' (Register Two  ) (Memory (_,_,v,_,_,_)) = v
get' (Register Three) (Memory (_,_,_,v,_,_)) = v
get' (Register Four ) (Memory (_,_,_,_,v,_)) = v
get' (Register Five ) (Memory (_,_,_,_,_,v)) = v

ipToMem :: State s => s -> s
ipToMem s = withMemory (set' (register s) (Immediate $ ip s) (memory s)) s

memToIp :: State s => s -> s
memToIp s = withIp (get (Register $ register s) s) s

incIp :: State s => s -> s
incIp s = withIp (1 + ip s) s

apply :: State s => s -> Step s -> s
apply s (Op (Add a b c)) = set c (get (Register a) s   +   get b s) s
apply s (Op (Mul a b c)) = set c (get (Register a) s   *   get b s) s
apply s (Op (Div a b c)) = set c (get (Register a) s `div` get b s) s
apply s (Op (Ban a b c)) = set c (get (Register a) s  .&.  get b s) s
apply s (Op (Bor a b c)) = set c (get (Register a) s  .|.  get b s) s
apply s (Op (Set a   c)) = set c (get a s) s
apply s (Op (Gt  a b c)) = set c (if get a s > get b s then 1 else 0) s
apply s (Op (Eq  a b c)) = set c (if get a s == get b s then 1 else 0) s
apply s (Op Noop)        = s
apply s (Custom f)       = f s

current :: State s => Program s -> s -> Step s
current (Program p) s = p !! ip s

terminated :: State s => Program s -> s -> Bool
terminated (Program instructions) s = ip s < 0 || length instructions <= ip s

runUntil :: (State s, Show s) => (Program s -> s -> Bool) -> Program s -> s -> s
runUntil done p s =
   if terminated p s || done p s
   then s
   else runUntil done p $ runOne s
   where
      runOne = incIp . memToIp . applyCurrent . ipToMem
      applyCurrent s' = s' `seq` apply s' (current p s')

run :: (Show s, State s) => Program s -> s -> Int
run p s =
   let s' = runUntil terminated p s
   in  get (Register Zero) s'
