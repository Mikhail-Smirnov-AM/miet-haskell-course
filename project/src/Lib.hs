module Lib where

-- функция, описывающая систему дифференциальных уравнений
-- du/dt = v
-- dv/dt = f(u)
-- к этой системе сводится дифференциальное уравнение второго порядка:
-- d2u/dt2 - f(u) = 0
diff_sys:: (a -> a) -> (a,a) -> (a,a)
diff_sys f (u,du) = (du, f u) 

-- один шаг метода Рунге-Кутты
runge_kutta_step :: Fractional a => (a->a) -> (a,a) -> a -> (a,a)
runge_kutta_step f (u_n,du_n) h = let (u1,du1) = diff_sys f (u_n,du_n)
                                      (u2,du2) = diff_sys f (u_n + h*u1/2, du_n + h*du1/2)
                                      (u3,du3) = diff_sys f (u_n + h*u2/2, du_n + h*du2/2)
                                      (u4,du4) = diff_sys f (u_n + h*u3, du_n + h*du3)
                                      u_n1 = u_n + h*(u1 + 2*u2 + 2*u3 + u4)/6
                                      du_n1 = du_n + h*(du1 + 2*du2 + 2*du3 + du4)/6
                                  in (u_n1,du_n1)

-- метод Рунге-Кутты
runge_kutta :: (Fractional a, Ord a) => (a->a) -> (a,a) -> a -> a -> a -> [(a,a)]
runge_kutta f (u0,du0) t b h = let (u_n,du_n) = runge_kutta_step f (u0,du0) h
                               in if (b-t < h) || (t > b)
                                  then [(u_n,du_n)]
                                  else ((u_n,du_n): runge_kutta f (u_n,du_n) (t+h) b h)

-- вычисление значений u(b) при начальных условиях u(a) = u0 u'(a) = du0
-- причем значение du0 варьируется в пределах 0,max_du0
find_ub :: (Fractional a, Ord a) => (a->a) -> (a,a) -> (a,a) -> a -> a -> [(a,a)]
find_ub f (u0,du0) (a,b) h max_du0 = let (u_b, _) = last $ runge_kutta f (u0,du0) (a+h) b h
                                     in if du0 < max_du0
                                        then ((du0,u_b): find_ub f (u0,du0 + h) (a,b) h max_du0)
                                        else [(du0,u_b)]

-- поиск интервала, которому принадлежит ноль функции u(b,u0,du0)
find_interval :: (Num a, Ord a) => [(a,a)] -> (a,a)
find_interval [] = (0,0)
find_interval (x0:xs) = let x1 = head xs
                           in if ((snd x0 < 0) && (snd x1 > 0)) || ((snd x0 > 0) && (snd x1 < 0)) 
                              then (fst x0, fst x1)
                              else find_interval xs 
                       

dychotomy :: (Fractional a, Ord a) => (a->a) -> a -> (a,a) -> a -> (a,a) -> a -> a
dychotomy f u0 (a,b) h (x1,x2) eps = let x_m = (x1+x2)/2
                                         (f1, _) = last $ runge_kutta f (u0,x1) (a+h) b h 
                                         (f_m, _) = last $ runge_kutta f (u0,x_m) (a+h) b h
                                     in if x2-x1 > eps
                                        then if f1*f_m < 0 
                                             then dychotomy f u0 (a,b) h (x1,x_m) eps  
                                             else dychotomy f u0 (a,b) h (x_m,x2) eps
                                        else x_m

shooting_method :: (Floating a, Ord a) => (a->a) -> (a,a) -> a -> a -> a -> a -> [(a,a)]
shooting_method f (a,b) h max_du0 u0 eps = let u_b = find_ub f (u0,0) (a,b) h max_du0
                                               interval = find_interval u_b
                                           in if interval == (0,0)
                                              then []
                                              else let du0 = dychotomy f u0 (a,b) h interval eps
                                                   in ((u0,du0):runge_kutta f (u0,du0) (a+h) b h)

       