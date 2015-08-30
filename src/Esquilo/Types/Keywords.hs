{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module Esquilo.Types.Keywords where
import GHC.TypeLits

-- http://www.postgresql.org/docs/9.0/static/sql-select.html
-- [ WITH [ RECURSIVE ] with_query [, ...] ]
-- SELECT [ ALL | DISTINCT [ ON ( expression [, ...] ) ] ]
--     * | expression [ [ AS ] output_name ] [, ...]
--     [ FROM from_item [, ...] ]
--     [ WHERE condition ]
--     [ GROUP BY expression [, ...] ]
--     [ HAVING condition [, ...] ]
--     [ WINDOW window_name AS ( window_definition ) [, ...] ]
--     [ { UNION | INTERSECT | EXCEPT } [ ALL ] select ]
--     [ ORDER BY expression [ ASC | DESC | USING operator ] [ NULLS { FIRST | LAST } ] [, ...] ]
--     [ LIMIT { count | ALL } ]
--     [ OFFSET start [ ROW | ROWS ] ]
--     [ FETCH { FIRST | NEXT } [ count ] { ROW | ROWS } ONLY ]
--     [ FOR { UPDATE | SHARE } [ OF table_name [, ...] ] [ NOWAIT ] [...] ]

data SELECT (a :: k) from (tbl :: Symbol)
data FROM
data WHERE
data (:=)
