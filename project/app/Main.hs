module Main (main) where

import Lib

main :: IO ()
main = do 
       let a = 0
       let b = 2*pi
       let h = 1e-2
       let max_du0 = 1.1
       let u0 = 0
       let eps = 1e-6
       let u_b = find_ub (\u -> (sinh u)/2-u) (u0,0) (a,b) h max_du0
       print u_b
       let interval = find_interval u_b
       print interval
       let du0 = dychotomy (\u -> (sinh u)/2-u) u0 (a,b) h interval eps
       print du0
