module Network.HTCPCP.Returns where

---------------------------------------
-- Returns from rfc2324
--
-- 406 Not Acceptable (
--  Server can't handle the request,
--  server should reply with list of
--  available coffee additions
-- )
-- 418 I'm a teapot (
--  If you send a Coffee request to a teapot
-- )
---------------------------------------

data PotErrorResponse = NA | TPOT

instance Show PotErrorResponse where
    show x =
        case x of
          NA -> "406 Not Acceptable"
          TPOT -> "418 I'm a Teapot"

