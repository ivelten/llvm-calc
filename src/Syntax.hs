module Syntax (Annotation, Location, Operator, Expression) where

class Annotation a where
  start :: a -> Int
  end   :: a -> Int

data Location = Location Int Int
  deriving stock (Eq, Ord, Show)

instance Annotation Location where
  start (Location s _) = s
  end (Location _ e) = e

data Operator = Add | Multiply | Subtract | Divide
  deriving stock (Eq, Ord, Show)

data Expression a
  = Value a Float
  | Infix a Operator (Expression a) (Expression a)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Annotation a => Annotation (Expression a) where
  start (Value ann _) = start ann
  start (Infix ann _ _ _) = start ann
  end (Value ann _) = end ann
  end (Infix ann _ _ _) = end ann
