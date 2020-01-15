module Practice where

-- 15.9.1
-- 1 + (2*3)
-- 2*3 が最内簡約化
--
-- (1+2) * (2+3)
-- 1+2, 2+3が最内簡約化
--
-- fst (1+2, 2+3)
-- fst が最外簡約化
--
-- (\x -> 1 + x) (2*3)
-- \x -> 1 + x が最外簡約化。(xを(2*3)で置き換える)