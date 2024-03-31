module SimpleLang where
-- Язык Simple -- очень простой императивный язык.
-- В нём только один тип данных: целые числа.

data Expression =
    Var String                   -- Переменные
  | Val Int                      -- Целые константы
  | Op Expression Bop Expression -- Бинарные операции
  deriving (Show, Eq)

data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt       -- >
  | Ge       -- >=
  | Lt       -- <
  | Le       -- <=
  | Eql      -- ==
  deriving (Show, Eq)

data Statement =
    -- присвоить переменной значение выражения
    Assign   String     Expression
    -- увеличить переменную на единицу
  | Incr     String
    -- ненулевые значения работают как истина в if, while и for
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
    -- как { ... } в C-подобных языках
  | Block [Statement]
    -- пустая инструкция
  | Skip
  deriving (Show, Eq)

-- примеры программ на этом языке в конце модуля

-- по состоянию можно получить значение каждой переменной
-- (в реальной программе скорее использовалось бы Data.Map.Map String Int)
type State = String -> Int

-- Задание 1 -----------------------------------------

-- в начальном состоянии все переменные имеют значение 0
empty :: State
empty = const 0

-- возвращает состояние, в котором переменная var имеет значение newVal, 
-- все остальные -- то же, что в state
extend :: State -> String -> Int -> State
extend state var newVal = \x -> if x == var then newVal else state x

-- Задание 2 -----------------------------------------

-- возвращает значение выражения expr при значениях переменных из state.
eval :: State -> Expression -> Int
eval state expr = case expr of
                  Var x -> state x
                  Val y -> y
                  Op left operation right -> case operation of
                                             Plus -> (eval state left) + (eval state right)
                                             Minus -> (eval state left) - (eval state right)
                                             Times -> (eval state left) * (eval state right)
                                             Divide -> quot (eval state left) (eval state right)
                                             Gt -> fromEnum ((eval state left) > (eval state right))
                                             Ge -> fromEnum ((eval state left) >= (eval state right))
                                             Lt -> fromEnum ((eval state left) < (eval state right))
                                             Le -> fromEnum ((eval state left) <= (eval state right))
                                             Eql -> fromEnum ((eval state left) == (eval state right))

-- Задание 3 -----------------------------------------

-- Можно выразить Incr через Assign, For через While, Block через 
-- последовательное выполнение двух инструкций (; в C).
-- Следующий тип задаёт упрощённый набор инструкций (промежуточный язык Simpler).
data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

-- упрощает программу Simple
desugar :: Statement -> DietStatement
desugar s = case s of
            Assign x ex -> DAssign x ex
            Incr x -> DAssign x (Op (Var x) Plus (Val 1))
            If ex then_s else_s -> DIf ex (desugar then_s) (desugar else_s)
            While ex while_s -> DWhile ex (desugar while_s)
            For begin_s end_ex step_s for_s -> DSequence (desugar begin_s) (DWhile end_ex (DSequence (desugar for_s) (desugar step_s)))
            Block [] -> DSkip
            Block (s0:ss) -> DSequence (desugar s0) (desugar (Block ss))
            Skip -> DSkip

-- Задание 4 -----------------------------------------

-- принимает начальное состояние и программу Simpler
-- и возвращает состояние после работы программы
runSimpler :: State -> DietStatement -> State
runSimpler state diet_statement = case diet_statement of
                                  DAssign x ex -> extend state x (eval state ex)  
                                  DIf if_ex then_s else_s -> if (eval state if_ex) == 1
                                                             then runSimpler state then_s
                                                             else runSimpler state else_s
                                  DWhile ex s -> if (eval state ex) == 1
                                                 then runSimpler (runSimpler state s) (DWhile ex s)
                                                 else state
                                  DSequence s1 s2 -> runSimpler (runSimpler state s1) s2 
                                  DSkip -> state
-- 
-- in s "A" ~?= 10

-- принимает начальное состояние и программу Simple
-- и возвращает состояние после работы программы
run :: State -> Statement -> State
run state statement = runSimpler state (desugar statement)

-- Программы -------------------------------------------

{- Вычисление факториала

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Вычисление целой части квадратного корня

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = Block [(Assign "B" (Val 0)),
                    (While (Op (Var "A") Ge (Op (Var "B") Times (Var "B"))) (Incr "B")),
                    (Assign "B" (Op (Var "B") Minus (Val 1)))]

{- Вычисление числа Фибоначчи

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = Block [(Assign "F0" (Val 1)),
                   (Assign "F1" (Val 1)),
                   (If (Op (Var "In") Eql (Val 0)) 
                       (Assign "Out" (Var "F0")) 
                       (If (Op (Var "In") Eql (Val 1)) 
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2)) (Op (Var "C") Le (Var "In")) (Incr "C") 
                                (Block [(Assign "T" (Op (Var "F0") Plus (Var "F1"))),
                                        (Assign "F0" (Var "F1")),
                                        (Assign "F1" (Var "T")),
                                        (Assign "Out" (Var "T"))                                                    
                                       ]
                                )
                           )                                 
                         )
                    )
                   ]
