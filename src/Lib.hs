module Lib
    ( 
    ) where


data Lens a b s t = Lens{view :: s -> a, update :: (b, s) -> t}


-- | example : there is a lens onto the left component of a pair (see note π1)
pi1 :: Lens a b (a, c) (b,c)
pi1 = Lens view update where 
    view (x,y) = x 

    update (x', (x,y)) = (x', y)

{- note [π1] 
 pi1 is the lens whose view function extracts the first component of a pair 
 and whose update overwrites the first component 
-}

-- | another example 
sign :: Lens Bool Bool Integer Integer 
sign = Lens view update 
    where 
        view x = x >= 0 

        update (b, x) = if b then abs x else (-1) * (abs x)

{- note [sign]
    sign is a lens onto a boolean within an integer; the view fn extracts the sign, and the 
    update fn enforces a new sign while preserving the absolute value.
    We should note that while the π1 lens was polymorphic the sign lens is monomorphic; i.e the 
    boolean sign can be replaced only with another boolean, not with a value of a different type. 
-}



data Prism a b s t = Prism{match :: s -> Either t a, build :: b -> t}


the :: Prism a b (Maybe a) (Maybe b)
the = Prism match build 
    where 
        match :: Maybe -> Either 
        match (Just x) = Right x
        match Nothing  = Left Nothing 

        build x = Just x 