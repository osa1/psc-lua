
-- Modified version of Prelude for Lua backend

module Prelude where

  flip :: forall a b c. (a -> b -> c) -> b -> a -> c
  flip f b a = f a b

  const :: forall a b. a -> b -> a
  const a _ = a

  asTypeOf :: forall a. a -> a -> a
  asTypeOf x _ = x

  infixr 9 >>>
  infixr 9 <<<

  class Semigroupoid a where
    (<<<) :: forall b c d. a c d -> a b c -> a b d

  instance semigroupoidArr :: Semigroupoid (->) where
    (<<<) f g x = f (g x)

  (>>>) :: forall a b c d. (Semigroupoid a) => a b c -> a c d -> a b d
  (>>>) f g = g <<< f

  class (Semigroupoid a) <= Category a where
    id :: forall t. a t t

  instance categoryArr :: Category (->) where
    id x = x

  infixr 0 $
  infixl 0 #

  ($) :: forall a b. (a -> b) -> a -> b
  ($) f x = f x

  (#) :: forall a b. a -> (a -> b) -> b
  (#) x f = f x

  infixr 6 :

  (:) :: forall a. a -> [a] -> [a]
  (:) = cons

  foreign import cons
    "local function cons(e)\n\
    \    return function (l)\n\
    \        local ret = {}\n\
    \        table.insert(ret, e)\n\
    \        for _, v in ipairs(l) do\n\
    \            table.insert(ret, v)\n\
    \        end\n\
    \        return ret\n\
    \    end\n\
    \end" :: forall a. a -> [a] -> [a]

  class Show a where
    show :: a -> String

  foreign import showStringImpl
    "local function showStringImpl(s)\n\
    \    return table.concat{\"\\\"\", s, \"\\\"\"}\n\
    \end" :: String -> String

  instance showString :: Show String where
    show = showStringImpl

  instance showBoolean :: Show Boolean where
    show true = "true"
    show false = "false"

  -- TODO: is there a way to make this private? (e.g. not export)
  foreign import toString
    "local function toString(a)\n\
    \   return tostring(a)\n\
    \end" :: forall a. a -> String

  instance showNumber :: Show Number where
    show = toString

  foreign import showArrayImpl
    "local function showArrayImpl(show)\n\
    \    return function(arr)\n\
    \        local strs = {}\n\
    \        for _, v in ipairs(arr) do\n\
    \            table.insert(strs, show(v))\n\
    \        end\n\
    \        return (\"[\" .. table.concat(strs, \", \") .. \"]\")\n\
    \    end\n\
    \end" :: forall a. (a -> String) -> [a] -> String

  instance showArray :: (Show a) => Show [a] where
    show = showArrayImpl show

  infixl 4 <$>

  class Functor f where
    (<$>) :: forall a b. (a -> b) -> f a -> f b

  infixl 4 <*>

  class (Functor f) <= Apply f where
    (<*>) :: forall a b. f (a -> b) -> f a -> f b

  class (Apply f) <= Applicative f where
    pure :: forall a. a -> f a

  liftA1 :: forall f a b. (Applicative f) => (a -> b) -> f a -> f b
  liftA1 f a = pure f <*> a

  infixl 3 <|>

  class Alternative f where
    empty :: forall a. f a
    (<|>) :: forall a. f a -> f a -> f a

  infixl 1 >>=

  class (Apply m) <= Bind m where
    (>>=) :: forall a b. m a -> (a -> m b) -> m b

  class (Applicative m, Bind m) <= Monad m

  return :: forall m a. (Monad m) => a -> m a
  return = pure

  liftM1 :: forall m a b. (Monad m) => (a -> b) -> m a -> m b
  liftM1 f a = do
    a' <- a
    return (f a')

  ap :: forall m a b. (Monad m) => m (a -> b) -> m a -> m b
  ap f a = do
    f' <- f
    a' <- a
    return (f' a')

  infixl 7 *
  infixl 7 /
  infixl 7 %

  infixl 6 -
  infixl 6 +

  class Num a where
    (+) :: a -> a -> a
    (-) :: a -> a -> a
    (*) :: a -> a -> a
    (/) :: a -> a -> a
    (%) :: a -> a -> a
    negate :: a -> a

  foreign import numAdd
    "local function numAdd(n1)\n\
    \    return function (n2)\n\
    \        return n1 + n2\n\
    \    end\n\
    \end" :: Number -> Number -> Number

  foreign import numSub
    "local function numSub(n1)\n\
    \    return function (n2)\n\
    \        return n1 - n2\n\
    \    end\n\
    \end" :: Number -> Number -> Number

  foreign import numMul
    "local function numMul(n1)\n\
    \    return function (n2)\n\
    \        return n1 * n2\n\
    \    end\n\
    \end" :: Number -> Number -> Number

  foreign import numDiv
    "local function numDiv(n1)\n\
    \    return function (n2)\n\
    \        return n1 / n2\n\
    \    end\n\
    \end" :: Number -> Number -> Number

  foreign import numMod
    "local function numMod(n1)\n\
    \    return function (n2)\n\
    \        return n1 % n2\n\
    \    end\n\
    \end" :: Number -> Number -> Number

  foreign import numNegate
    "local function numNegate(n)\n\
    \    return -n1\n\
    \end" :: Number -> Number

  instance numNumber :: Num Number where
    (+) = numAdd
    (-) = numSub
    (*) = numMul
    (/) = numDiv
    (%) = numMod
    negate = numNegate

  infix 4 ==
  infix 4 /=

  class Eq a where
    (==) :: a -> a -> Boolean
    (/=) :: a -> a -> Boolean

  foreign import refEq
    "local function refEq(r1)\n\
    \    return function (r2)\n\
    \        return r1 == r2\n\
    \    end\n\
    \end" :: forall a. a -> a -> Boolean

  foreign import refIneq
    "local function refIneq(r1)\n\
    \    return function (r2)\n\
    \        return r1 ~= r2\n\
    \    end\n\
    \end" :: forall a. a -> a -> Boolean

  instance eqString :: Eq String where
    (==) = refEq
    (/=) = refIneq

  instance eqNumber :: Eq Number where
    (==) = refEq
    (/=) = refIneq

  instance eqBoolean :: Eq Boolean where
    (==) = refEq
    (/=) = refIneq

  instance eqArray :: (Eq a) => Eq [a] where
    (==) [] [] = true
    (==) (x:xs) (y:ys) = x == y && xs == ys
    (==) _ _ = false
    (/=) xs ys = not (xs == ys)

  data Ordering = LT | GT | EQ

  instance eqOrdering :: Eq Ordering where
    (==) LT LT = true
    (==) GT GT = true
    (==) EQ EQ = true
    (==) _  _  = false
    (/=) x y = not (x == y)

  instance showOrdering :: Show Ordering where
    show LT = "LT"
    show GT = "GT"
    show EQ = "EQ"

  class (Eq a) <= Ord a where
    compare :: a -> a -> Ordering

  infixl 4 <

  (<) :: forall a. (Ord a) => a -> a -> Boolean
  (<) a1 a2 = case a1 `compare` a2 of
    LT -> true
    _ -> false

  infixl 4 >

  (>) :: forall a. (Ord a) => a -> a -> Boolean
  (>) a1 a2 = case a1 `compare` a2 of
    GT -> true
    _ -> false

  infixl 4 <=

  (<=) :: forall a. (Ord a) => a -> a -> Boolean
  (<=) a1 a2 = case a1 `compare` a2 of
    GT -> false
    _ -> true

  infixl 4 >=

  (>=) :: forall a. (Ord a) => a -> a -> Boolean
  (>=) a1 a2 = case a1 `compare` a2 of
    LT -> false
    _ -> true

  foreign import numCompare
    "local function numCompare(n1)\n\
    \    return function (n2)\n\
    \        if n1 < n2 then\n\
    \            return LT\n\
    \        elseif n1 == n2 then\n\
    \            return EQ\n\
    \        else\n\
    \            return GT\n\
    \        end\n\
    \    end\n\
    \end" :: Number -> Number -> Ordering

  instance ordNumber :: Ord Number where
    compare = numCompare

  infixl 10 &
  infixl 10 |
  infixl 10 ^

  class Bits b where
    (&) :: b -> b -> b
    (|) :: b -> b -> b
    (^) :: b -> b -> b
    shl :: b -> Number -> b
    shr :: b -> Number -> b
    zshr :: b -> Number -> b
    complement :: b -> b

  infixr 2 ||
  infixr 3 &&

  class BoolLike b where
    (&&) :: b -> b -> b
    (||) :: b -> b -> b
    not :: b -> b

  foreign import boolAnd
    "local function boolAnd(b1)\n\
    \    return function (b2)\n\
    \        return b1 and b2\n\
    \    end\n\
    \end" :: Boolean -> Boolean -> Boolean



  foreign import boolOr
    "local function boolOr(b1)\n\
    \    return function(b2)\n\
    \        return b1 or b2\n\
    \    end\n\
    \end" :: Boolean -> Boolean -> Boolean

  foreign import boolNot
    "local function boolNot(b)\n\
    \    return not b\n\
    \end" :: Boolean -> Boolean

  instance boolLikeBoolean :: BoolLike Boolean where
    (&&) = boolAnd
    (||) = boolOr
    not = boolNot

  infixr 5 <>

  class Semigroup a where
    (<>) :: a -> a -> a

  foreign import concatString
    "local function concatString(s1)\n\
    \    return function(s2)\n\
    \        return s1 .. s2\n\
    \    end\n\
    \end" :: String -> String -> String

  instance semigroupString :: Semigroup String where
    (<>) = concatString

  infixr 5 ++

  (++) :: forall s. (Semigroup s) => s -> s -> s
  (++) = (<>)

module Data.Function where

  on :: forall a b c. (b -> b -> c) -> (a -> b) -> a -> a -> c
  on f g x y = g x `f` g y

module Data.Eq where

  data Ref a = Ref a

  liftRef :: forall a b. (a -> a -> b) -> Ref a -> Ref a -> b
  liftRef f (Ref x) (Ref y) = f x y

  instance eqRef :: Eq (Ref a) where
    (==) = liftRef refEq
    (/=) = liftRef refIneq

module Prelude.Unsafe where

  foreign import unsafeIndex
    "local function unsafeIndex(arr)\n\
    \    return function (n)\n\
    \        return arr[n+1]\n\
    \    end\n\
    \end" :: forall a. [a] -> Number -> a

module Control.Monad.Eff where

  foreign import data Eff :: # ! -> * -> *

  foreign import returnE
    "local function returnE(a)\n\
    \    return function ()\n\
    \        return a\n\
    \    end\n\
    \end" :: forall e a. a -> Eff e a

  foreign import bindE
    "local function bindE(a)\n\
    \    return function (f)\n\
    \        return function ()\n\
    \            return f(a())()\n\
    \        end\n\
    \    end\n\
    \end" :: forall e a b. Eff e a -> (a -> Eff e b) -> Eff e b

  type Pure a = forall e. Eff e a

  foreign import runPure
    "local function runPure(f)\n\
    \    return f()\n\
    \end" :: forall a. Pure a -> a

  instance functorEff :: Functor (Eff e) where
    (<$>) = liftA1

  instance applyEff :: Apply (Eff e) where
    (<*>) = ap

  instance applicativeEff :: Applicative (Eff e) where
    pure = returnE

  instance bindEff :: Bind (Eff e) where
    (>>=) = bindE

  instance monadEff :: Monad (Eff e)

  foreign import untilE
    "local function untilE(f)\n\
    \    return function ()\n\
    \        while not f() do end\n\
    \        return {}\n\
    \    end\n\
    \end" :: forall e. Eff e Boolean -> Eff e {}

  foreign import whileE
    "local function whileE(f)\n\
    \    return function(a)\n\
    \        return function()\n\
    \            while f() do a() end\n\
    \            return {}\n\
    \        end\n\
    \    end\n\
    \end" :: forall e a. Eff e Boolean -> Eff e a -> Eff e {}

  foreign import forE
    "local function forE(lo)\n\
    \    return function (hi)\n\
    \        return function (f)\n\
    \            return function ()\n\
    \                for i=lo, hi do f(i)() end\n\
    \            end\n\
    \        end\n\
    \    end\n\
    \end" :: forall e. Number -> Number -> (Number -> Eff e {}) -> Eff e {}

  foreign import foreachE
    "local function foreachE(arr)\n\
    \    return function (f)\n\
    \        for _, v in ipairs(arr) do\n\
    \            f(v)()\n\
    \        end\n\
    \    end\n\
    \end" :: forall e a. [a] -> (a -> Eff e {}) -> Eff e {}

module Control.Monad.Eff.Unsafe where

  import Control.Monad.Eff

  foreign import unsafeInterleaveEff
    "local function unsafeInterleaveEff(f)\n\
    \    return f\n\
    \end" :: forall eff1 eff2 a. Eff eff1 a -> Eff eff2 a

module Debug.Trace where

  import Control.Monad.Eff

  foreign import data Trace :: !

  foreign import trace
    "local function trace(s)\n\
    \    return function ()\n\
    \        print(s)\n\
    \        return {}\n\
    \    end\n\
    \end" :: forall r. String -> Eff (trace :: Trace | r) {}

  print :: forall a r. (Show a) => a -> Eff (trace :: Trace | r) {}
  print o = trace (show o)

module Control.Monad.ST where

  import Control.Monad.Eff

  foreign import data ST :: * -> !

  foreign import data STRef :: * -> * -> *

  foreign import data STArray :: * -> * -> *

  foreign import newSTRef
    "local function newSTRef(val)\n\
    \    return function ()\n\
    \        return { value = val }\n\
    \    end\n\
    \end" :: forall a h r. a -> Eff (st :: ST h | r) (STRef h a)

  foreign import readSTRef
    "local function readSTRef(ref)\n\
    \    return function ()\n\
    \        return ref.value\n\
    \    end\n\
    \end" :: forall a h r. STRef h a -> Eff (st :: ST h | r) a

  foreign import modifySTRef
    "local function modifySTRef(ref)\n\
    \    return function (f)\n\
    \        return function ()\n\
    \            local ret = f(ref.value)\n\
    \            ref.value = ret\n\
    \            return ret\n\
    \        end\n\
    \    end\n\
    \end" :: forall a h r. STRef h a -> (a -> a) -> Eff (st :: ST h | r) a

  foreign import writeSTRef
    "local function writeSTRef(ref)\n\
    \    return function (a)\n\
    \        return function ()\n\
    \            ref.value = a\n\
    \            return a\n\
    \        end\n\
    \    end\n\
    \end" :: forall a h r. STRef h a -> a -> Eff (st :: ST h | r) a

  foreign import newSTArray
    "local function newSTArray(len)\n\
    \    return function (a)\n\
    \        return function ()\n\
    \            local arr = {}\n\
    \            for i=1, len do\n\
    \                arr[i] = a\n\
    \            end\n\
    \            return arr\n\
    \        end\n\
    \    end\n\
    \end" :: forall a h r. Number -> a -> Eff (st :: ST h | r) (STArray h a)

  foreign import peekSTArray
    "local function peekSTArray(arr)\n\
    \    return function (i)\n\
    \        return function ()\n\
    \            return arr[i+1]\n\
    \        end\n\
    \    end\n\
    \end" :: forall a h r. STArray h a -> Eff (st :: ST h | r) a

  foreign import pokeSTArray
    "local function pokeSTArray(arr)\n\
    \    return function (i)\n\
    \        return function (a)\n\
    \            return function ()\n\
    \                arr[i+1] = a\n\
    \                return a\n\
    \            end\n\
    \        end\n\
    \    end\n\
    \end" :: forall a h r. STArray h a -> Number -> a -> Eff (st :: ST h | r) a

  foreign import runST
    "local function runST(f)\n\
    \    return f\n\
    \end" :: forall a r. (forall h. Eff (st :: ST h | r) a) -> Eff r a

  foreign import runSTArray
    "local function runSTArray(f)\n\
    \    return f\n\
    \end" :: forall a r. (forall h. Eff (st :: ST h | r) (STArray h a)) -> Eff r [a]
