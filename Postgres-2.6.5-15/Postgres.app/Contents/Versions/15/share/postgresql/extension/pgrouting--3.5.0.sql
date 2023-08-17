
\echo Use "CREATE EXTENSION pgrouting" to load this file. \quit




---
--- pgRouting provides geospatial routing functionality.
--- http://pgrouting.org
--- copyright
--- -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
--- Functions are under GNU General Public Licence.

---
--- This program is free software; you can redistribute it and/or modify
--- it under the terms of the GNU General Public License as published by
--- the Free Software Foundation; either version 2 of the License, or
--- (at your option) any later version.

--- This program is distributed in the hope that it will be useful,
--- but WITHOUT ANY WARRANTY; without even the implied warranty of
--- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
--- GNU General Public License for more details.

--- You should have received a copy of the GNU General Public License
--- along with this program; if not, write to the Free Software
--- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

--- -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
---
--- The following functions have MIT-X licence:
---  pgr_analyzeGraph
---  pgr_analizeOneWay

--- This is free software, you can redistribute and/or modify it:
--- the terms of the GNU General Public Licence. See the COPYING file.
--- the terms of the MIT-X Licence. See the COPYING file.
---
--- -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
--
-- WARNING: Any change in this file must be evaluated for compatibility.
--          Changes cleanly handled by pgrouting upgrades.sql are fine,
--          other changes will require a bump in Major version.
--          Currently only function replaceble by CREATE OR REPLACE
--          are cleanly handled.
--
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



--v2.6
CREATE FUNCTION _pgr_pointToId(
    point geometry,
    tolerance double precision,
    vertname text,
    srid integer)

  RETURNS bigint AS
$BODY$
DECLARE
    rec record;
    pid bigint;

BEGIN
    EXECUTE 'SELECT ST_Distance(
        the_geom,
        ST_GeomFromText(ST_AsText('
                || quote_literal(point::text)
                || '),'
            || srid ||')) AS d, id, the_geom
    FROM '||_pgr_quote_ident(vertname)||'
    WHERE ST_DWithin(
        the_geom,
        ST_GeomFromText(
            ST_AsText(' || quote_literal(point::text) ||'),
            ' || srid || '),' || tolerance||')
    ORDER BY d
    LIMIT 1' INTO rec ;
    IF rec.id IS NOT NULL THEN
        pid := rec.id;
    ELSE
        execute 'INSERT INTO '||_pgr_quote_ident(vertname)||' (the_geom) VALUES ('||quote_literal(point::text)||')';
        pid := lastval();
END IF;

RETURN pid;

END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;

COMMENT ON FUNCTION _pgr_pointToId(geometry, FLOAT, TEXT, INTEGER)
IS 'pgRouting internal function';



--v2.6
CREATE FUNCTION _pgr_startpoint(g geometry)
RETURNS geometry AS
$$
SELECT CASE WHEN geometryType($1) ~ '^MULTI' THEN ST_StartPoint(ST_geometryN($1,1))
ELSE ST_StartPoint($1)
END;
$$
LANGUAGE sql IMMUTABLE;

COMMENT ON FUNCTION _pgr_startPoint(geometry)
IS 'pgRouting internal function';


--v2.6
CREATE FUNCTION _pgr_endPoint(g geometry)
RETURNS geometry AS
$$
SELECT CASE WHEN geometryType($1) ~ '^MULTI' THEN ST_EndPoint(st_geometryN($1,1))
ELSE ST_EndPoint($1)
END;
$$
LANGUAGE sql IMMUTABLE;

COMMENT ON FUNCTION _pgr_endPoint(geometry)
IS 'pgRouting internal function';







--v2.6
CREATE FUNCTION _pgr_getTableName(IN tab text, IN reportErrs int default 0, IN fnName text default '_pgr_getTableName', OUT sname text,OUT tname text)
  RETURNS RECORD AS
$$
DECLARE
        naming record;
        i integer;
        query text;
        sn text; -- schema name
        tn text; -- table name
        ttype text; --table type for future use
        err boolean;
        debuglevel text;
        var_types text[] = ARRAY['BASE TABLE', 'VIEW'];
BEGIN

    execute 'show client_min_messages' into debuglevel;


    perform _pgr_msg( 0, fnName, 'Checking table ' || tab || ' exists');
    --RAISE DEBUG 'Checking % exists',tab;

    i := strpos(tab,'.');
    IF (i <> 0) THEN
        sn := split_part(tab, '.',1);
        tn := split_part(tab, '.',2);
    ELSE
        sn := current_schema;
        tn := tab;
    END IF;


   SELECT schema_name INTO sname
   FROM information_schema.schemata WHERE schema_name = sn;

    IF sname IS NOT NULL THEN -- found schema (as is)
       SELECT table_name, table_type INTO tname, ttype
       FROM information_schema.tables
       WHERE
                table_type = ANY(var_types) and
                table_schema = sname and
                table_name = tn ;
        IF tname is NULL THEN
            SELECT table_name, table_type INTO tname, ttype
            FROM information_schema.tables
            WHERE
                table_type  = ANY(var_types) and
                table_schema = sname and
                table_name = lower(tn) ORDER BY table_name;
        END IF;
    END IF;
    IF sname is NULL or tname is NULL THEN --schema not found or table not found
        SELECT schema_name INTO sname
        FROM information_schema.schemata
        WHERE schema_name = lower(sn) ;

        IF sname IS NOT NULL THEN -- found schema (with lower caps)
            SELECT table_name, table_type INTO tname, ttype
            FROM information_schema.tables
            WHERE
                table_type  =  ANY(var_types) and
                table_schema = sname and
                table_name= tn ;

           IF tname IS NULL THEN
                SELECT table_name, table_type INTO tname, ttype
                FROM information_schema.tables
                WHERE
                    table_type  =  ANY(var_types) and
                    table_schema = sname and
                    table_name= lower(tn) ;
           END IF;
        END IF;
    END IF;
   err = (sname IS NULL OR tname IS NULL);
   perform _pgr_onError(err, reportErrs, fnName, 'Table ' || tab ||' not found',' Check your table name', 'Table '|| tab || ' found');

END;
$$
LANGUAGE plpgsql VOLATILE STRICT;

COMMENT ON FUNCTION _pgr_getTableName(TEXT, INTEGER, TEXT)
IS 'pgRouting internal function';







--v2.6
CREATE FUNCTION _pgr_getColumnName(sname text, tname text, col text, IN reportErrs int default 1, IN fnName text default '_pgr_getColumnName')
RETURNS text AS
$BODY$
DECLARE
    cname text;
    naming record;
    err boolean;
BEGIN

    execute 'SELECT column_name FROM information_schema.columns
          WHERE table_name='||quote_literal(tname)||' and table_schema='||quote_literal(sname)||' and column_name='||quote_literal(col) into cname;

    IF cname is null  THEN
    execute 'SELECT column_name FROM information_schema.columns
          WHERE table_name='||quote_literal(tname)||' and table_schema='||quote_literal(sname)||' and column_name='||quote_literal(lower(col))  into cname;
    END if;

    err = cname is null;

    perform _pgr_onError(err, reportErrs, fnName,  'Column '|| col ||' not found', ' Check your column name','Column '|| col || ' found');
    RETURN cname;
END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;



--v2.6
CREATE FUNCTION _pgr_getColumnName(tab text, col text, IN reportErrs int default 1, IN fnName text default '_pgr_getColumnName')
RETURNS text AS
$BODY$
DECLARE
    sname text;
    tname text;
    cname text;
    naming record;
    err boolean;
BEGIN
    select * into naming from _pgr_getTableName(tab,reportErrs, fnName) ;
    sname=naming.sname;
    tname=naming.tname;

    select * into cname from _pgr_getColumnName(sname,tname,col,reportErrs, fnName);
    RETURN cname;
END;

$BODY$
LANGUAGE plpgsql VOLATILE STRICT;

COMMENT ON FUNCTION _pgr_getColumnName(TEXT, TEXT, TEXT, INTEGER, TEXT)
IS 'pgRouting internal function';
COMMENT ON FUNCTION _pgr_getColumnName(TEXT, TEXT, INTEGER, TEXT)
IS 'pgRouting internal function';


--v2.6
CREATE FUNCTION _pgr_isColumnInTable(tab text, col text)
RETURNS boolean AS
$BODY$
DECLARE
    cname text;
BEGIN
    select * from _pgr_getColumnName(tab,col,0, '_pgr_isColumnInTable') into cname;
    return cname is not null;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE STRICT;

COMMENT ON FUNCTION _pgr_isColumnInTable(TEXT, TEXT)
IS 'pgRouting internal function';



--v2.6
CREATE FUNCTION _pgr_isColumnIndexed(sname text, tname text, cname text,
      IN reportErrs int default 1, IN fnName text default '_pgr_isColumnIndexed')
RETURNS boolean AS
$BODY$
DECLARE
    naming record;
    rec record;
    pkey text;
BEGIN
    SELECT
          pg_attribute.attname into pkey
         --  format_type(pg_attribute.atttypid, pg_attribute.atttypmod)
          FROM pg_index, pg_class, pg_attribute
          WHERE
                  pg_class.oid = _pgr_quote_ident(sname||'.'||tname)::regclass AND
                  indrelid = pg_class.oid AND
                  pg_attribute.attrelid = pg_class.oid AND
                  pg_attribute.attnum = any(pg_index.indkey)
                  AND indisprimary;

    IF pkey=cname then
          RETURN TRUE;
    END IF;

    SELECT a.index_name,
           b.attname,
           b.attnum,
           a.indisunique,
           a.indisprimary
      INTO rec
      FROM ( SELECT a.indrelid,
                    a.indisunique,
                    a.indisprimary,
                    c.relname index_name,
                    unnest(a.indkey) index_num
               FROM pg_index a,
                    pg_class b,
                    pg_class c,
                    pg_namespace d
              WHERE b.relname=tname
                AND b.relnamespace=d.oid
                AND d.nspname=sname
                AND b.oid=a.indrelid
                AND a.indexrelid=c.oid
           ) a,
           pg_attribute b
     WHERE a.indrelid = b.attrelid
       AND a.index_num = b.attnum
       AND b.attname = cname
  ORDER BY a.index_name,
           a.index_num;

  RETURN FOUND;
  EXCEPTION WHEN OTHERS THEN
    perform _pgr_onError( true, reportErrs, fnName,
    'Error when checking for the postgres system attributes', SQLERR);
    RETURN FALSE;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE STRICT;

--v2.6
CREATE FUNCTION _pgr_isColumnIndexed(tab text, col text,
      IN reportErrs int default 1, IN fnName text default '_pgr_isColumnIndexed')
RETURNS boolean AS
$BODY$
DECLARE
    naming record;
    rec record;
    sname text;
    tname text;
    cname text;
    pkey text;
    value boolean;
BEGIN
    SELECT * into naming FROM _pgr_getTableName(tab, 0, fnName);
    sname=naming.sname;
    tname=naming.tname;
    IF sname IS NULL OR tname IS NULL THEN
        RETURN FALSE;
    END IF;
    SELECT * into cname from _pgr_getColumnName(sname, tname, col, 0, fnName) ;
    IF cname IS NULL THEN
        RETURN FALSE;
    END IF;
    select * into value  from _pgr_isColumnIndexed(sname, tname, cname, reportErrs, fnName);
    return value;
END
$BODY$
  LANGUAGE plpgsql VOLATILE STRICT;

COMMENT ON FUNCTION _pgr_isColumnIndexed(TEXT, TEXT, TEXT, INTEGER, TEXT)
IS 'pgRouting internal function';
COMMENT ON FUNCTION _pgr_isColumnIndexed(TEXT, TEXT, INTEGER, TEXT)
IS 'pgRouting internal function';


--v2.6
CREATE FUNCTION _pgr_quote_ident(idname text)
    returns text as
$body$
declare
    t text[];
    pgver text;

begin
    pgver := regexp_replace(version(), E'^PostgreSQL ([^ ]+)[ ,].*$', E'\\1');

    if _pgr_versionless(pgver, '9.2') then
        select into t array_agg(quote_ident(term)) from
            (select nullif(unnest, '') as term
               from unnest(string_to_array(idname, '.'))) as foo;
    else
        select into t array_agg(quote_ident(term)) from
            (select unnest(string_to_array(idname, '.', '')) as term) as foo;
    end if;
    return array_to_string(t, '.');
end;
$body$
language plpgsql immutable;

COMMENT ON FUNCTION _pgr_quote_ident(TEXT)
IS 'pgRouting internal function';


--v2.6
CREATE FUNCTION _pgr_versionless(v1 text, v2 text)
  RETURNS boolean AS
$BODY$


declare
    v1a text[];
    v2a text[];
    nv1 integer;
    nv2 integer;
    ne1 integer;
    ne2 integer;

begin
    -- separate components into an array, like:
    -- '2.1.0-beta3dev'  =>  {2,1,0,beta3dev}
    v1a := regexp_matches(v1, E'^(\\d+)(?:[\\.](\\d+))?(?:[\\.](\\d+))?[-+\\.]?(.*)$');
    v2a := regexp_matches(v2, E'^(\\d+)(?:[\\.](\\d+))?(?:[\\.](\\d+))?[-+\\.]?(.*)$');

    -- convert modifiers to numbers for comparison
    -- we do not delineate between alpha1, alpha2, alpha3, etc
    ne1 := case when v1a[4] is null or v1a[4]='' then 5
                when v1a[4] ilike 'rc%' then 4
                when v1a[4] ilike 'beta%' then 3
                when v1a[4] ilike 'alpha%' then 2
                when v1a[4] ilike 'dev%' then 1
                else 0 end;

    ne2 := case when v2a[4] is null or v2a[4]='' then 5
                when v2a[4] ilike 'rc%' then 4
                when v2a[4] ilike 'beta%' then 3
                when v2a[4] ilike 'alpha%' then 2
                when v2a[4] ilike 'dev%' then 1
                else 0 end;

    nv1 := v1a[1]::integer * 10000 +
           coalesce(v1a[2], '0')::integer * 1000 +
           coalesce(v1a[3], '0')::integer *  100 + ne1;
    nv2 := v2a[1]::integer * 10000 +
           coalesce(v2a[2], '0')::integer * 1000 +
           coalesce(v2a[3], '0')::integer *  100 + ne2;

    --raise notice 'nv1: %, nv2: %, ne1: %, ne2: %', nv1, nv2, ne1, ne2;

    return nv1 < nv2;
end;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 1;

COMMENT ON FUNCTION _pgr_versionless(TEXT, TEXT)
IS 'pgRouting internal function';





--v2.6
CREATE FUNCTION _pgr_parameter_check(fn text, sql text, big boolean default false)
  RETURNS bool AS
  $BODY$

  DECLARE
  rec record;
  rec1 record;
  has_rcost boolean;
  safesql text;
  BEGIN
    IF (big) THEN
       RAISE EXCEPTION 'This function is for old style functions';
    END IF;

    -- checking query is executable
    BEGIN
      safesql =  'select * from ('||sql||' ) AS __a__ limit 1';
      execute safesql into rec;
      EXCEPTION
        WHEN OTHERS THEN
            RAISE EXCEPTION 'Could not execute query please verify syntax of: '
              USING HINT = sql;
    END;

    -- checking the fixed columns and data types of the integers
    IF fn IN ('dijkstra','astar') THEN
        BEGIN
          execute 'select id,source,target,cost  from ('||safesql||') as __b__' into rec;
          EXCEPTION
            WHEN OTHERS THEN
                RAISE EXCEPTION 'An expected column was not found in the query'
                  USING ERRCODE = 'XX000',
                   HINT = 'Please veryfy the column names: id, source, target, cost';
        END;
        execute 'select pg_typeof(id)::text as id_type, pg_typeof(source)::text as source_type, pg_typeof(target)::text as target_type, pg_typeof(cost)::text as cost_type'
            || ' from ('||safesql||') AS __b__ ' into rec;
        -- Version 2.0.0 is more restrictive
        IF NOT(   (rec.id_type in ('integer'::text))
              AND (rec.source_type in ('integer'::text))
              AND (rec.target_type in ('integer'::text))
              AND (rec.cost_type = 'double precision'::text)) THEN
            RAISE EXCEPTION 'Error, columns ''source'', ''target'' must be of type int4, ''cost'' must be of type float8'
            USING ERRCODE = 'XX000';
        END IF;
    END IF;


    IF fn IN ('astar') THEN
        BEGIN
          execute 'select x1,y1,x2,y2  from ('||safesql||') as __b__' into rec;
          EXCEPTION
            WHEN OTHERS THEN
                RAISE EXCEPTION 'An expected column was not found in the query'
                  USING ERRCODE = 'XX000',
                   HINT = 'Please veryfy the column names: x1,y1, x2,y2';
        END;
        execute 'select pg_typeof(x1)::text as x1_type, pg_typeof(y1)::text as y1_type, pg_typeof(x2)::text as x2_type, pg_typeof(y2)::text as y2_type'
            || ' from ('||safesql||') AS __b__ ' into rec;
        -- Version 2.0.0 is more restrictive
        IF NOT(   (rec.x1_type = 'double precision'::text)
              AND (rec.y1_type = 'double precision'::text)
              AND (rec.x2_type = 'double precision'::text)
              AND (rec.y2_type = 'double precision'::text)) THEN
            RAISE EXCEPTION 'Columns: x1, y1, x2, y2 must be of type float8'
            USING ERRCODE = 'XX000';
        END IF;
    END IF;

    -- checking the fixed columns and data types of the integers
    IF fn IN ('johnson') THEN
        BEGIN
          execute 'select source,target,cost  from ('||safesql||') as __b__' into rec;
          EXCEPTION
            WHEN OTHERS THEN
                RAISE EXCEPTION 'An expected column was not found in the query'
                  USING HINT = 'Please veryfy the column names: id, source, target, cost',
                         ERRCODE = 'XX000';
        END;

        execute 'select pg_typeof(source)::text as source_type, pg_typeof(target)::text as target_type, pg_typeof(cost)::text as cost_type'
            || ' from ('||safesql||') AS __b__ ' into rec;
        -- Version 2.0.0 is more restrictive
        IF NOT(   (rec.source_type in ('integer'::text))
              AND (rec.target_type in ('integer'::text))
              AND (rec.cost_type = 'double precision'::text)) THEN
            RAISE EXCEPTION 'Support for source,target columns only of type: integer. Support for Cost: double precision'
            USING ERRCODE = 'XX000';
        END IF;
    END IF;


    -- Checking the data types of the optional reverse_cost";
    has_rcost := false;
    IF fn IN ('johnson','dijkstra','astar') THEN
      BEGIN
        execute 'select reverse_cost, pg_typeof(reverse_cost)::text as rev_type  from ('||safesql||' ) AS __b__ limit 1 ' into rec1;
        has_rcost := true;
        EXCEPTION
          WHEN OTHERS THEN
            has_rcost = false;
            return has_rcost;
      END;
      if (has_rcost) then
        IF (big) then
           IF  not (rec1.rev_type in ('bigint'::text, 'integer'::text, 'smallint'::text, 'double precision'::text, 'real'::text)) then
             RAISE EXCEPTION 'Illegar type in optional parameter reverse_cost.'
             USING ERRCODE = 'XX000';
           END IF;
        ELSE -- Version 2.0.0 is more restrictive
           IF (rec1.rev_type != 'double precision') then
             RAISE EXCEPTION 'Illegal type in optional parameter reverse_cost, must be of type float8'
             USING ERRCODE = 'XX000';
           END IF;
        END IF;
      end if;
      return true;
    END IF;
    -- just for keeps
    return true;
  END
  $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 1;

COMMENT ON FUNCTION _pgr_parameter_check(TEXT, TEXT, BOOLEAN)
IS 'pgRouting internal function';






--v2.6
CREATE FUNCTION _pgr_onError(
  IN errCond boolean,  -- true there is an error
  IN reportErrs int,   -- 0, 1 or 2
  IN fnName text,      -- function name that generates the error
  IN msgerr text,      -- error message
  IN hinto text default 'No hint', -- hint help
  IN msgok text default 'OK')      -- message if everything is ok
  RETURNS void AS
$BODY$
BEGIN
  if errCond=true then
     if reportErrs=0 then
       raise debug '----> PGR DEBUG in %: %',fnName,msgerr USING HINT = '  ---->'|| hinto;
     else
       if reportErrs = 2 then
         raise notice '----> PGR ERROR in %: %',fnName,msgerr USING HINT = '  ---->'|| hinto;
         raise raise_exception;
       else
         raise notice '----> PGR NOTICE in %: %',fnName,msgerr USING HINT = '  ---->'|| hinto;
       end if;
     end if;
  else
       raise debug 'PGR ----> %: %',fnName,msgok;
  end if;
END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;

COMMENT ON FUNCTION _pgr_onError(BOOLEAN, INTEGER, TEXT, TEXT, TEXT, TEXT)
IS 'pgRouting internal function';




--v2.6
CREATE FUNCTION _pgr_msg(IN msgKind int, IN fnName text, IN msg text default '---->OK')
  RETURNS void AS
$BODY$
BEGIN
  if msgKind = 0 then
       raise debug '----> PGR DEBUG in %: %',fnName,msg;
  else
       raise notice '----> PGR NOTICE in %: %',fnName,msg;
  end if;
END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;

COMMENT ON FUNCTION _pgr_msg(INTEGER, TEXT, TEXT)
IS 'pgRouting internal function';



--v2.6
CREATE FUNCTION _pgr_getColumnType(sname text, tname text, cname text,
     IN reportErrs int default 0, IN fnName text default '_pgr_getColumnType')
RETURNS text AS
$BODY$
DECLARE
    ctype text;
    naming record;
    err boolean;
BEGIN

    EXECUTE 'select data_type  from information_schema.columns '
            || 'where table_name = '||quote_literal(tname)
                 || ' and table_schema=' || quote_literal(sname)
                 || ' and column_name='||quote_literal(cname)
       into ctype;
    err = ctype is null;
    perform _pgr_onError(err, reportErrs, fnName,
            'Type of Column '|| cname ||' not found',
            'Check your column name',
            'OK: Type of Column '|| cname || ' is ' || ctype);
    RETURN ctype;
END;

$BODY$
LANGUAGE plpgsql VOLATILE STRICT;


--v2.6
CREATE FUNCTION _pgr_getColumnType(tab text, col text,
     IN reportErrs int default 0, IN fnName text default '_pgr_getColumnType')
RETURNS text AS
$BODY$
DECLARE
    sname text;
    tname text;
    cname text;
    ctype text;
    naming record;
    err boolean;
BEGIN

    select * into naming from _pgr_getTableName(tab,reportErrs, fnName) ;
    sname=naming.sname;
    tname=naming.tname;
    select * into cname from _pgr_getColumnName(tab,col,reportErrs, fnName) ;
    select * into ctype from _pgr_getColumnType(sname,tname,cname,reportErrs, fnName);
    RETURN ctype;
END;

$BODY$
LANGUAGE plpgsql VOLATILE STRICT;


COMMENT ON FUNCTION _pgr_getColumnType(TEXT, TEXT, TEXT, INTEGER, TEXT)
IS 'pgRouting internal function';

COMMENT ON FUNCTION _pgr_getColumnType(TEXT, TEXT, INTEGER, TEXT)
IS 'pgRouting internal function';




--v2.6
CREATE FUNCTION _pgr_get_statement(o_sql text)
RETURNS text AS
$BODY$
DECLARE
sql TEXT;
BEGIN
    EXECUTE 'SELECT statement FROM pg_prepared_statements WHERE name ='  || quote_literal(o_sql) || ' limit 1 ' INTO sql;
    IF (sql IS NULL) THEN
      RETURN   o_sql;
    ELSE
      RETURN  regexp_replace(regexp_replace(regexp_replace(sql, '\s(as)\s', '___foo___', 'i'), '^.*___foo___', '','i'), ';$', '');
    END IF;
END
$BODY$
LANGUAGE plpgsql STABLE STRICT;

COMMENT ON FUNCTION _pgr_get_statement(TEXT)
IS 'pgRouting internal function';


--v2.6
CREATE FUNCTION _pgr_checkVertTab(vertname text, columnsArr  text[],
    IN reportErrs int default 1, IN fnName text default '_pgr_checkVertTab',
    OUT sname text,OUT vname text)
RETURNS record AS
$BODY$
DECLARE
    cname text;
    colname text;
    naming record;
    debuglevel text;
    err  boolean;
    msgKind int;

BEGIN
    msgKind = 0; -- debug_
    execute 'show client_min_messages' into debuglevel;

    perform _pgr_msg(msgKind, fnName, 'Checking table ' || vertname || ' exists');
       select * from _pgr_getTableName(vertname, 0, fnName) into naming;
       sname=naming.sname;
       vname=naming.tname;
       err = sname is NULL or vname is NULL;
    perform _pgr_onError( err, 2, fnName,
          'Vertex Table: ' || vertname || ' not found',
          'Please create ' || vertname || ' using  _pgr_createTopology() or pgr_createVerticesTable()',
          'Vertex Table: ' || vertname || ' found');


    perform _pgr_msg(msgKind, fnName, 'Checking columns of ' || vertname);
      FOREACH cname IN ARRAY columnsArr
      loop
         select _pgr_getcolumnName(vertname, cname, 0, fnName) into colname;
         if colname is null then
           perform _pgr_msg(msgKind, fnName, 'Adding column ' || cname || ' in ' || vertname);
           set client_min_messages  to warning;
                execute 'ALTER TABLE '||_pgr_quote_ident(vertname)||' ADD COLUMN '||cname|| ' integer';
           execute 'set client_min_messages  to '|| debuglevel;
           perform _pgr_msg(msgKind, fnName);
         end if;
      end loop;
    perform _pgr_msg(msgKind, fnName, 'Finished checking columns of ' || vertname);

    perform _pgr_createIndex(vertname , 'id' , 'btree', reportErrs, fnName);
 END
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;

COMMENT ON FUNCTION _pgr_checkVertTab(TEXT, TEXT[], INTEGER, TEXT)
IS 'pgRouting internal function';







--v2.6
CREATE FUNCTION _pgr_createIndex(
    sname text, tname text, colname text, indext text,
    IN reportErrs int default 1, IN fnName text default '_pgr_createIndex')
RETURNS void AS
$BODY$
DECLARE
    debuglevel text;
    naming record;
    tabname text;
    query text;
    msgKind int;
BEGIN
  msgKind = 0; -- debug_

  execute 'show client_min_messages' into debuglevel;
  tabname=_pgr_quote_ident(sname||'.'||tname);
  perform _pgr_msg(msgKind, fnName, 'Checking ' || colname || ' column in ' || tabname || ' is indexed');
    IF (_pgr_isColumnIndexed(sname,tname,colname, 0, fnName)) then
       perform _pgr_msg(msgKind, fnName);
    else
      if indext = 'gist' then
        query = 'create  index '||_pgr_quote_ident(tname||'_'||colname||'_idx')||'
                         on '||tabname||' using gist('||quote_ident(colname)||')';
      else
        query = 'create  index '||_pgr_quote_ident(tname||'_'||colname||'_idx')||'
                         on '||tabname||' using btree('||quote_ident(colname)||')';
      end if;
      perform _pgr_msg(msgKind, fnName, 'Adding index ' || tabname || '_' ||  colname || '_idx');
      perform _pgr_msg(msgKind, fnName, ' Using ' ||  query);
      set client_min_messages  to warning;
      BEGIN
        execute query;
        EXCEPTION WHEN others THEN
          perform _pgr_onError( true, reportErrs, fnName,
            'Could not create index on:' || colname, SQLERRM);
      END;
      execute 'set client_min_messages  to '|| debuglevel;
      perform _pgr_msg(msgKind, fnName);
    END IF;
END;

$BODY$
  LANGUAGE plpgsql VOLATILE STRICT;


--v2.6
CREATE FUNCTION _pgr_createIndex(tabname text, colname text, indext text,
    IN reportErrs int default 1, IN fnName text default '_pgr_createIndex')
RETURNS void AS
$BODY$
DECLARE
    naming record;
    sname text;
    tname text;

BEGIN
    select * from _pgr_getTableName(tabname, 2, fnName)  into naming;
    sname=naming.sname;
    tname=naming.tname;
    execute _pgr_createIndex(sname, tname, colname, indext, reportErrs, fnName);
END;

$BODY$
  LANGUAGE plpgsql VOLATILE STRICT;


COMMENT ON FUNCTION _pgr_createIndex(TEXT, TEXT, TEXT, TEXT, INTEGER, TEXT)
IS 'pgRouting internal function';

COMMENT ON FUNCTION _pgr_createIndex(TEXT, TEXT, TEXT, INTEGER, TEXT)
IS 'pgRouting internal function';


--v3.4
CREATE FUNCTION _pgr_checkquery(
  TEXT  -- SQL
)
RETURNS TEXT AS
$BODY$
DECLARE
  main_sql TEXT;

BEGIN

  BEGIN
    EXECUTE format($$
      SELECT regexp_replace(regexp_replace(statement, %1$L,'','i'),';$','')
      FROM pg_prepared_statements WHERE name = %2$L$$,
      '.*' || $1 || '\s*as', $1)
    INTO main_sql;

    EXCEPTION WHEN OTHERS
      THEN main_sql := $1;
  END;

  IF main_sql IS NULL THEN
    main_sql := $1;
  END IF;

  BEGIN
    EXECUTE format('SELECT * FROM ( %1$s ) AS __a__ limit 1', main_sql);

    EXCEPTION WHEN OTHERS THEN
      RAISE EXCEPTION '%', SQLERRM
      USING HINT = 'Please check query: '|| $1;
  END;

  RETURN main_sql;

END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;


-- COMMENTS
COMMENT ON FUNCTION _pgr_checkquery(TEXT)
IS '_pgr_checkquery is an internal function';



--v3.4
CREATE FUNCTION _pgr_checkColumn(
  TEXT, -- SQL
  TEXT, -- Column
  TEXT, -- Types
  is_optional BOOLEAN DEFAULT false,
  dryrun BOOLEAN DEFAULT false
)
RETURNS BOOLEAN AS
$BODY$
DECLARE
  has_column BOOLEAN := TRUE;
  rec RECORD;

BEGIN

  BEGIN
    EXECUTE format('SELECT %1$s FROM ( %2$s ) AS __a__ limit 1', $2, $1);

    EXECUTE format('SELECT pg_typeof(%1$s) FROM ( %2$s ) AS __a__ limit 1', $2, $1)
    INTO rec;

    EXCEPTION WHEN OTHERS THEN
      has_column := FALSE;
  END;

  IF dryrun THEN
    RETURN has_column;
  END IF;

  IF NOT is_optional AND NOT has_column THEN
    RAISE EXCEPTION 'Missing column'
    USING HINT = format('Column "%1$s" missing in: %2$s', $2, $1);
  END IF;

  IF has_column THEN
    CASE $3
    WHEN 'ANY-INTEGER' THEN
      IF  rec.pg_typeof NOT IN ('smallint','integer','bigint') THEN
        RAISE EXCEPTION 'Expected type of column "%" is ANY-INTEGER', $2
        USING HINT = 'Query: ' || $1;
      END IF;
    WHEN 'ANY-INTEGER[]' THEN
      IF  rec.pg_typeof NOT IN ('smallint[]','integer[]','bigint[]') THEN
        RAISE EXCEPTION 'Expected type of column "%" is ANY-INTEGER-ARRAY', $2
        USING HINT = 'Query: ' || $1;
      END IF;
    WHEN 'ANY-NUMERICAL' THEN
      IF  rec.pg_typeof NOT IN ('smallint','integer','bigint','real','float','numeric') THEN
        RAISE EXCEPTION 'Expected type of column "%s" is ANY-NUMERICAL', $2
        USING HINT = 'Query: ' || $1;
      END IF;
    ELSE
      IF rec.pg_typeof::TEXT != $3 THEN
        RAISE EXCEPTION 'Expected type of column "%" is %', $2, $3
        USING HINT = 'Query: ' || $1;
      END IF;
    END CASE;
  END IF;

  RETURN has_column;

END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;


-- COMMENTS
COMMENT ON FUNCTION _pgr_checkColumn(TEXT, TEXT, TEXT, BOOLEAN, BOOLEAN)
IS '_pgr_checkColumn is an internal function';


---------------
---------------
-- dijkstra
---------------
---------------

--v3.2
CREATE FUNCTION _pgr_dijkstra(
    edges_sql TEXT,
    start_vids ANYARRAY,
    end_vids ANYARRAY,
    directed BOOLEAN,
    only_cost BOOLEAN,
    normal BOOLEAN,
    n_goals BIGINT,
    global BOOLEAN,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE C VOLATILE STRICT;


--v3.2
CREATE FUNCTION _pgr_dijkstra(
    edges_sql TEXT,
    combinations_sql TEXT,
    directed BOOLEAN,
    only_cost BOOLEAN,
    n_goals BIGINT,
    global BOOLEAN,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE C VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_dijkstra(TEXT, ANYARRAY, ANYARRAY, BOOLEAN, BOOLEAN, BOOLEAN, BIGINT, BOOLEAN)
IS 'pgRouting internal function';

COMMENT ON FUNCTION _pgr_dijkstra(TEXT, TEXT, BOOLEAN, BOOLEAN, BIGINT, BOOLEAN)
IS 'pgRouting internal function';



--v3.0
CREATE FUNCTION _pgr_dijkstra(
    edges_sql TEXT,
    start_vids ANYARRAY,
    end_vids ANYARRAY,
    directed BOOLEAN DEFAULT true,
    only_cost BOOLEAN DEFAULT false,
    normal BOOLEAN DEFAULT true,
    n_goals BIGINT DEFAULT 0,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE C VOLATILE STRICT;


--v3.1
CREATE FUNCTION _pgr_dijkstra(
    edges_sql TEXT,
    combinations_sql TEXT,
    directed BOOLEAN DEFAULT true,
    only_cost BOOLEAN DEFAULT false,
    normal BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE C VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_dijkstra(TEXT, ANYARRAY, ANYARRAY, BOOLEAN, BOOLEAN, BOOLEAN, BIGINT)
IS 'pgRouting internal function';

COMMENT ON FUNCTION _pgr_dijkstra(TEXT, TEXT, BOOLEAN, BOOLEAN, BOOLEAN)
IS 'pgRouting internal function';


---------------
-- pgr_dijkstra
---------------

-- ONE to ONE
--v2.6
CREATE FUNCTION pgr_dijkstra(
    TEXT,   -- edges_sql (required)
    BIGINT, -- from_vid (required)
    BIGINT, -- to_vid (required)

    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_dijkstra(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], ARRAY[$3]::BIGINT[], $4, false, true) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;

-- ONE to MANY
--v2.6
CREATE FUNCTION pgr_dijkstra(
    TEXT,     -- edges_sql (required)
    BIGINT,   -- from_vid (required)
    ANYARRAY, -- to_vids (required)

    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_dijkstra(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], $3::BIGINT[], $4, false, true) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;

-- MANY to ONE
--v2.6
CREATE FUNCTION pgr_dijkstra(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    BIGINT,   -- to_vid (required)

    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_dijkstra(_pgr_get_statement($1), $2::BIGINT[], ARRAY[$3]::BIGINT[], $4, false, false) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;

-- MANY to MANY
--v2.6
CREATE FUNCTION pgr_dijkstra(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    ANYARRAY, -- to_vids (required)

    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_dijkstra(_pgr_get_statement($1), $2::BIGINT[], $3::BIGINT[], $4, false, true) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;

-- Combinations SQL signature
--v3.1
CREATE FUNCTION pgr_dijkstra(
    TEXT,     -- edges_sql (required)
    TEXT,     -- combinations_sql (required)

    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_dijkstra(_pgr_get_statement($1), _pgr_get_statement($2), $3, false, true) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- COMMENTS

COMMENT ON FUNCTION pgr_dijkstra(TEXT, BIGINT, BIGINT, BOOLEAN)
IS 'pgr_dijkstra(One to One)
- Parameters:
   - Edges SQL with columns: id, source, target, cost [,reverse_cost]
   - From vertex identifier
   - To vertex identifier
- Optional Parameters
   - directed := true
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_dijkstra.html
';

COMMENT ON FUNCTION pgr_dijkstra(TEXT, BIGINT, ANYARRAY, BOOLEAN)
IS 'pgr_dijkstra(One to Many)
- Parameters:
   - Edges SQL with columns: id, source, target, cost [,reverse_cost]
   - From vertex identifier
   - To ARRAY[vertices identifiers]
- Optional Parameters
   - directed := true
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_dijkstra.html
';

COMMENT ON FUNCTION pgr_dijkstra(TEXT, ANYARRAY, BIGINT, BOOLEAN)
IS 'pgr_dijkstra(Many to One)
- Parameters:
   - Edges SQL with columns: id, source, target, cost [,reverse_cost]
   - From ARRAY[vertices identifiers]
   - To vertex identifier
- Optional Parameters
   - directed := true
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_dijkstra.html
';

COMMENT ON FUNCTION pgr_dijkstra(TEXT, ANYARRAY, ANYARRAY, BOOLEAN)
IS 'pgr_dijkstra(Many to Many)
- Parameters:
   - Edges SQL with columns: id, source, target, cost [,reverse_cost]
   - From ARRAY[vertices identifiers]
   - To ARRAY[vertices identifiers]
- Optional Parameters
   - directed := true
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_dijkstra.html
';

COMMENT ON FUNCTION pgr_dijkstra(TEXT, TEXT, BOOLEAN)
IS 'pgr_dijkstra(Combinations)
- Parameters:
   - Edges SQL with columns: id, source, target, cost [,reverse_cost]
   - Combinations SQL with columns: source, target
- Optional Parameters
   - directed := true
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_dijkstra.html
';


-------------------
-- pgr_dijkstraCost
-------------------

-- ONE to ONE
--v2.6
CREATE FUNCTION pgr_dijkstraCost(
    TEXT,   -- edges_sql (required)
    BIGINT, -- from_vids (required)
    BIGINT, -- to_vids (required)

    directed BOOLEAN DEFAULT true,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT start_vid, end_vid, agg_cost
    FROM _pgr_dijkstra(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], ARRAY[$3]::BIGINT[], $4, true);
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;



-- ONE to MANY
--v2.6
CREATE FUNCTION pgr_dijkstraCost(
    TEXT,     -- edges_sql (required)
    BIGINT,   -- from_vid (required)
    ANYARRAY, -- to_vids (required)

    directed BOOLEAN DEFAULT true,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT start_vid, end_vid, agg_cost
    FROM _pgr_dijkstra(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], $3::BIGINT[], $4, true);
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;




-- MANY to ONE
--v2.6
CREATE FUNCTION pgr_dijkstraCost(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    BIGINT,   -- to_vid (required)

    directed BOOLEAN DEFAULT true,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT start_vid, end_vid, agg_cost
    FROM _pgr_dijkstra(_pgr_get_statement($1), $2::BIGINT[], ARRAY[$3]::BIGINT[], $4, true);
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;




-- MANY to MANY
--v2.6
CREATE FUNCTION pgr_dijkstraCost(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    ANYARRAY, -- to_vids (required)

    directed BOOLEAN DEFAULT true,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT start_vid, end_vid, agg_cost
    FROM _pgr_dijkstra(_pgr_get_statement($1), $2::BIGINT[], $3::BIGINT[], $4, true);
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;

-- Combinations SQL signature
--v3.1
CREATE FUNCTION pgr_dijkstraCost(
    TEXT,     -- edges_sql (required)
    TEXT,     -- combinations_sql (required)

    directed BOOLEAN DEFAULT true,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT start_vid, end_vid, agg_cost
    FROM _pgr_dijkstra(_pgr_get_statement($1), _pgr_get_statement($2), $3, true, true);
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- COMMENTS

COMMENT ON FUNCTION pgr_dijkstraCost(TEXT, BIGINT, BIGINT, BOOLEAN)
IS 'pgr_dijkstraCost(One to One)
- Parameters:
   - Edges SQL with columns: id, source, target, cost [,reverse_cost]
   - From vertex identifier
   - To vertex identifier
- Optional Parameters
   - directed := true
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_dijkstraCost.html
';

COMMENT ON FUNCTION pgr_dijkstraCost(TEXT, BIGINT, ANYARRAY, BOOLEAN)
IS 'pgr_dijkstraCost(One to Many)
- Parameters:
   - Edges SQL with columns: id, source, target, cost [,reverse_cost]
   - From vertex identifier
   - To ARRAY[vertices identifiers]
- Optional Parameters
   - directed := true
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_dijkstraCost.html
';

COMMENT ON FUNCTION pgr_dijkstraCost(TEXT, ANYARRAY, BIGINT, BOOLEAN)
IS 'pgr_dijkstraCost(Many to One)
- Parameters:
   - Edges SQL with columns: id, source, target, cost [,reverse_cost]
   - From ARRAY[vertices identifiers]
   - To vertex identifier
- Optional Parameters
   - directed := true
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_dijkstraCost.html
';

COMMENT ON FUNCTION pgr_dijkstraCost(TEXT, ANYARRAY, ANYARRAY, BOOLEAN)
IS 'pgr_dijkstraCost(Many to Many)
- Parameters:
   - Edges SQL with columns: id, source, target, cost [,reverse_cost]
   - From ARRAY[vertices identifiers]
   - To ARRAY[vertices identifiers]
- Optional Parameters
   - directed := true
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_dijkstraCost.html
';

COMMENT ON FUNCTION pgr_dijkstraCost(TEXT, TEXT, BOOLEAN)
IS 'pgr_dijkstraCost(Combinations SQL)
- Parameters:
   - Edges SQL with columns: id, source, target, cost [,reverse_cost]
   - Combinations SQL with columns: source, target
- Optional Parameters
   - directed := true
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_dijkstraCost.html
';


-----------------------------
-- dijkstraCostMatrix
-----------------------------


--v2.6
CREATE FUNCTION pgr_dijkstraCostMatrix(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- vids (required)

    directed BOOLEAN DEFAULT true,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.start_vid, a.end_vid, a.agg_cost
    FROM _pgr_dijkstra(_pgr_get_statement($1), $2, $2, $3, TRUE) a;
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;

-- COMMENT

COMMENT ON FUNCTION pgr_dijkstraCostMatrix(TEXT, ANYARRAY, BOOLEAN)
IS 'pgr_dijkstraCostMatrix
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - ARRAY [vertices identifiers]
- Optional Parameters
    - directed := true
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_dijkstraCostMatrix.html
';


------------------
-- pgr_dijkstraVia
------------------

--v3.0
CREATE FUNCTION _pgr_dijkstraVia(
    edges_sql TEXT,
    via_vids ANYARRAY,
    directed BOOLEAN,
    strict BOOLEAN,
    U_turn_on_edge BOOLEAN,


    OUT seq INTEGER,
    OUT path_id INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT,
    OUT route_agg_cost FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_dijkstraVia(TEXT, ANYARRAY, BOOLEAN, BOOLEAN, BOOLEAN)
IS 'pgRouting internal function';


--v2.6
CREATE FUNCTION pgr_dijkstraVia(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- via_vids (required)

    directed BOOLEAN DEFAULT true,
    strict BOOLEAN DEFAULT false,
    U_turn_on_edge BOOLEAN DEFAULT true,


    OUT seq INTEGER,
    OUT path_id INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT,
    OUT route_agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_dijkstraVia(_pgr_get_statement($1), $2, $3 , $4, $5);
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;

-- COMMENTS

COMMENT ON FUNCTION pgr_dijkstraVia(TEXT, ANYARRAY, BOOLEAN, BOOLEAN, BOOLEAN)
IS 'pgr_dijkstraVia
- PROPOSED
- Parameters:
   - Edges SQL with columns: id, source, target, cost [,reverse_cost]
   - ARRAY[via vertices identifiers]
- Optional Parameters
   - directed := true
   - strict := false
   - U_turn_on_edge := true
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_dijkstraVia.html
';


--------------------
-- _pgr_dijkstraNear
--------------------


-- ONE to MANY
--v3.0
CREATE FUNCTION _pgr_dijkstraNear(
    TEXT,     -- edges_sql (required)
    BIGINT,   -- from_vid (required)
    ANYARRAY, -- to_vids (required)
    BIGINT,   -- stop_at (required)

    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT seq, path_seq, end_vid, node, edge, cost, agg_cost
    FROM _pgr_dijkstra(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], $3::BIGINT[], $5, false, true, $4);
$BODY$
LANGUAGE sql VOLATILE
COST 100
ROWS 1000;


-- MANY to ONE
--v3.0
CREATE FUNCTION _pgr_dijkstraNear(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    BIGINT,   -- to_vid (required)
    BIGINT,   -- stop_at (required)

    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT seq, path_seq, start_vid, node, edge, cost, agg_cost
    FROM _pgr_dijkstra(_pgr_get_statement($1), $2::BIGINT[], ARRAY[$3]::BIGINT[], $5, false, false, $4);
$BODY$
LANGUAGE sql VOLATILE
COST 100
ROWS 1000;

-- MANY to MANY
--v3.0
CREATE FUNCTION _pgr_dijkstraNear(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    ANYARRAY, -- to_vids (required)
    BIGINT,   -- stop_at (required)

    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT end_vid BIGINT,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT seq, path_seq, start_vid, end_vid, node, edge, cost, agg_cost
    FROM _pgr_dijkstra(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], ARRAY[$3]::BIGINT[], $5, false, false, $4);
$BODY$
LANGUAGE sql VOLATILE
COST 100
ROWS 1000;

-- COMMENTS

COMMENT ON FUNCTION _pgr_dijkstraNear(TEXT, BIGINT, ANYARRAY, BIGINT, BOOLEAN)
IS 'pgRouting internal function';

COMMENT ON FUNCTION  _pgr_dijkstraNear(TEXT, ANYARRAY, BIGINT, BIGINT, BOOLEAN)
IS 'pgRouting internal function';

COMMENT ON FUNCTION  _pgr_dijkstraNear(TEXT, ANYARRAY, ANYARRAY, BIGINT, BOOLEAN)
IS 'pgRouting internal function';


--------------------
-- pgr_dijkstraNear
--------------------

-- ONE to MANY
--v3.2
CREATE FUNCTION pgr_dijkstraNear(
    TEXT,     -- edges_sql (required)
    BIGINT,   -- from_vid (required)
    ANYARRAY, -- to_vids (required)

    directed BOOLEAN DEFAULT true,
    cap      BIGINT DEFAULT 1,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT seq, path_seq, start_vid, end_vid, node, edge, cost, agg_cost
    FROM _pgr_dijkstra(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], $3::BIGINT[], directed, false, true, cap);
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;

-- MANY to ONE
--v3.2
CREATE FUNCTION pgr_dijkstraNear(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    BIGINT,   -- to_vid (required)

    directed BOOLEAN DEFAULT true,
    cap      BIGINT DEFAULT 1,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT seq, path_seq, start_vid, end_vid, node, edge, cost, agg_cost
    FROM _pgr_dijkstra(_pgr_get_statement($1), $2::BIGINT[], ARRAY[$3]::BIGINT[], directed, false, false, cap);
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;

-- MANY to MANY
--v3.2
CREATE FUNCTION pgr_dijkstraNear(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    ANYARRAY, -- to_vids (required)

    directed BOOLEAN DEFAULT true,
    cap      BIGINT DEFAULT 1,
    global   BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT seq, path_seq, start_vid, end_vid, node, edge, cost, agg_cost
    FROM _pgr_dijkstra(_pgr_get_statement($1), $2::BIGINT[], $3::BIGINT[], directed, false, true, cap, global);
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;

-- Combinations SQL signature
--v3.2
CREATE FUNCTION pgr_dijkstraNear(
    TEXT,     -- edges_sql (required)
    TEXT,     -- combinations_sql (required)

    directed BOOLEAN DEFAULT true,
    cap      BIGINT DEFAULT 1,
    global   BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT seq, path_seq, start_vid, end_vid, node, edge, cost, agg_cost
    FROM _pgr_dijkstra(_pgr_get_statement($1), _pgr_get_statement($2), directed, false, cap, global);
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- COMMENTS

COMMENT ON FUNCTION pgr_dijkstraNear(TEXT, BIGINT, ANYARRAY, BOOLEAN, BIGINT)
IS 'pgr_dijkstraNear(One to Many)
- PROPOSED
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From vertex identifier
  - To ARRAY[vertices identifiers]
- Optional Parameters
  - directed => true
  - cap => 1 (nth found)
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_dijkstraNear.html
';

COMMENT ON FUNCTION  pgr_dijkstraNear(TEXT, ANYARRAY, BIGINT, BOOLEAN, BIGINT)
IS 'pgr_dijkstraNear(Many to One)
- PROPOSED
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From ARRAY[vertices identifiers]
  - To vertex identifier
- Optional Parameters
  - directed => true
  - cap => 1 (nth found)
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_dijkstraNear.html
';

COMMENT ON FUNCTION  pgr_dijkstraNear(TEXT, ANYARRAY, ANYARRAY, BOOLEAN, BIGINT, BOOLEAN)
IS 'pgr_dijkstraNear(Many to Many)
- PROPOSED
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From ARRAY[vertices identifiers]
  - To ARRAY[vertices identifiers]
- Optional Parameters
  - directed => true
  - cap => 1 (nth found)
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_dijkstraNear.html
';

COMMENT ON FUNCTION  pgr_dijkstraNear(TEXT, TEXT, BOOLEAN, BIGINT, BOOLEAN)
IS 'pgr_dijkstraNear(Combinations)
- PROPOSED
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - Combinations SQL with columns: source, target
- Optional Parameters
  - directed => true
  - cap => 1 (nth found)
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_dijkstraNear.html
';


--------------------
-- pgr_dijkstraNearCost
--------------------

-- ONE to MANY
--v3.2
CREATE FUNCTION pgr_dijkstraNearCost(
    TEXT,     -- edges_sql (required)
    BIGINT,   -- from_vid (required)
    ANYARRAY, -- to_vids (required)

    directed BOOLEAN DEFAULT true,
    cap      BIGINT DEFAULT 1,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT start_vid, end_vid, agg_cost
    FROM _pgr_dijkstra(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], $3::BIGINT[], directed, true, true, cap, true);
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;

-- MANY to ONE
--v3.2
CREATE FUNCTION pgr_dijkstraNearCost(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    BIGINT,   -- to_vid (required)

    directed BOOLEAN DEFAULT true,
    cap      BIGINT DEFAULT 1,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT start_vid, end_vid, agg_cost
    FROM _pgr_dijkstra(_pgr_get_statement($1), $2::BIGINT[], ARRAY[$3]::BIGINT[], directed, true, false, cap, true);
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;

-- MANY to MANY
--v3.2
CREATE FUNCTION pgr_dijkstraNearCost(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    ANYARRAY, -- to_vids (required)

    directed BOOLEAN DEFAULT true,
    cap      BIGINT DEFAULT 1,
    global   BOOLEAN DEFAULT true,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT start_vid, end_vid, agg_cost
    FROM _pgr_dijkstra(_pgr_get_statement($1), $2::BIGINT[], $3::BIGINT[], directed, true, true, cap, global);
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;

-- Combinations SQL signature
--v3.2
CREATE FUNCTION pgr_dijkstraNearCost(
    TEXT,     -- edges_sql (required)
    TEXT,     -- combinations_sql (required)

    directed BOOLEAN DEFAULT true,
    cap      BIGINT DEFAULT 1,
    global   BOOLEAN DEFAULT true,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT start_vid, end_vid, agg_cost
    FROM _pgr_dijkstra(_pgr_get_statement($1), _pgr_get_statement($2), directed, true, cap, global);
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- COMMENTS

COMMENT ON FUNCTION pgr_dijkstraNearCost(TEXT, BIGINT, ANYARRAY, BOOLEAN, BIGINT)
IS 'pgr_dijkstraNearCost(One to Many)
- PROPOSED
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From vertex identifier
  - To ARRAY[vertices identifiers]
- Optional Parameters
  - directed => true
  - cap => 1 (nth found)
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_dijkstraNearCost.html
';

COMMENT ON FUNCTION  pgr_dijkstraNearCost(TEXT, ANYARRAY, BIGINT, BOOLEAN, BIGINT)
IS 'pgr_dijkstraNearCost(Many to One)
- PROPOSED
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From ARRAY[vertices identifiers]
  - To vertex identifier
- Optional Parameters
  - directed => true
  - cap => 1 (nth found)
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_dijkstraNearCost.html
';

COMMENT ON FUNCTION  pgr_dijkstraNearCost(TEXT, ANYARRAY, ANYARRAY, BOOLEAN, BIGINT, BOOLEAN)
IS 'pgr_dijkstraNearCost(Many to Many)
- PROPOSED
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From ARRAY[vertices identifiers]
  - To ARRAY[vertices identifiers]
- Optional Parameters
  - directed => true
  - cap => 1 (nth found)
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_dijkstraNearCost.html
';

COMMENT ON FUNCTION  pgr_dijkstraNearCost(TEXT, TEXT, BOOLEAN, BIGINT, BOOLEAN)
IS 'pgr_dijkstraNearCost(Combinations)
- PROPOSED
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - Combinations SQL with columns: source, target
- Optional Parameters
  - directed => true
  - cap => 1 (nth found)
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_dijkstraNearCost.html
';


------------------
-- pgr_johnson
------------------

--v3.0
CREATE FUNCTION _pgr_johnson(
    edges_sql TEXT,
    directed BOOLEAN,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE C VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_johnson(TEXT, BOOLEAN)
IS 'pgRouting internal function';


--v2.6
CREATE FUNCTION pgr_johnson(
    TEXT,    -- edges_sql (required)
    directed BOOLEAN DEFAULT true,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$

    SELECT *
    FROM _pgr_johnson(_pgr_get_statement($1), $2);

$BODY$
LANGUAGE SQL VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION pgr_johnson(TEXT, BOOLEAN)
IS 'pgr_johnson
- Parameters:
    - edges SQL with columns: source, target, cost [,reverse_cost])
- Optional Parameters:
    - directed := true
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_johnson.html
';


---------------------
-- pgr_floydWarshall
---------------------

--v3.0
CREATE FUNCTION _pgr_floydWarshall(
    edges_sql TEXT,
    directed BOOLEAN,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE C VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_floydWarshall(TEXT, BOOLEAN)
IS 'pgRouting internal function';


--v2.6
CREATE FUNCTION pgr_floydWarshall(
    TEXT,    -- edges_sql (required)
    directed BOOLEAN DEFAULT true,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$

    SELECT *
    FROM _pgr_floydWarshall(_pgr_get_statement($1), $2);

$BODY$
LANGUAGE SQL VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION pgr_floydWarshall(TEXT, BOOLEAN)
IS 'pgr_floydWarshall
- Parameters:
    - edges SQL with columns: source, target, cost [,reverse_cost])
- Optional Parameters:
    - directed := true
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_floydWarshall.html
';


-----------------
-----------------
-- _astar
-----------------
-----------------


-----------------
-- pgr_astar
-----------------


--v2.6
CREATE FUNCTION _pgr_astar(
    edges_sql TEXT, -- XY edges sql
    start_vids ANYARRAY,
    end_vids ANYARRAY,

    directed BOOLEAN DEFAULT true,
    heuristic INTEGER DEFAULT 5,
    factor FLOAT DEFAULT 1.0,
    epsilon FLOAT DEFAULT 1.0,
    only_cost BOOLEAN DEFAULT false,
    normal BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c VOLATILE STRICT;


--v3.2
CREATE FUNCTION _pgr_astar(
    edges_sql TEXT, -- XY edges sql
    combinations_sql TEXT,

    directed BOOLEAN DEFAULT true,
    heuristic INTEGER DEFAULT 5,
    factor FLOAT DEFAULT 1.0,
    epsilon FLOAT DEFAULT 1.0,
    only_cost BOOLEAN DEFAULT false,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_astar(TEXT, ANYARRAY, ANYARRAY, BOOLEAN, INTEGER, FLOAT, FLOAT, BOOLEAN, BOOLEAN)
IS 'pgRouting internal function';

COMMENT ON FUNCTION _pgr_astar(TEXT, TEXT, BOOLEAN, INTEGER, FLOAT, FLOAT, BOOLEAN)
IS 'pgRouting internal function';


-----------------
-- pgr_aStar
-----------------


--v2.6
CREATE FUNCTION pgr_aStar(
    TEXT,     -- edges sql (required)
    BIGINT,   -- from_vid (required)
    BIGINT,   -- to_vid (required)

    directed BOOLEAN DEFAULT true,
    heuristic INTEGER DEFAULT 5,
    factor FLOAT DEFAULT 1.0,
    epsilon FLOAT DEFAULT 1.0,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_aStar(_pgr_get_statement($1), ARRAY[$2]::BIGINT[],  ARRAY[$3]::BIGINT[], $4, $5, $6::FLOAT, $7::FLOAT) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


--v2.6
CREATE FUNCTION pgr_aStar(
    TEXT,       -- edges sql (required)
    BIGINT,     -- from_vid (required)
    ANYARRAY,   -- to_vids (required)

    directed BOOLEAN DEFAULT true,
    heuristic INTEGER DEFAULT 5,
    factor FLOAT DEFAULT 1.0,
    epsilon FLOAT DEFAULT 1.0,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.end_vid, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_aStar(_pgr_get_statement($1), ARRAY[$2]::BIGINT[],  $3::BIGINT[], $4, $5, $6::FLOAT, $7::FLOAT) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


--v2.6
CREATE FUNCTION pgr_aStar(
    TEXT,       -- edges sql (required)
    ANYARRAY,   -- from_vids (required)
    BIGINT,     -- to_vid (required)

    directed BOOLEAN DEFAULT true,
    heuristic INTEGER DEFAULT 5,
    factor FLOAT DEFAULT 1.0,
    epsilon FLOAT DEFAULT 1.0,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.start_vid, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_aStar(_pgr_get_statement($1), $2::BIGINT[],  ARRAY[$3]::BIGINT[], $4, $5, $6::FLOAT, $7::FLOAT, normal:=false) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


--v2.6
CREATE FUNCTION pgr_aStar(
    TEXT,       -- edges sql (required)
    ANYARRAY,   -- from_vids (required)
    ANYARRAY,   -- to_vids (required)

    directed BOOLEAN DEFAULT true,
    heuristic INTEGER DEFAULT 5,
    factor FLOAT DEFAULT 1.0,
    epsilon FLOAT DEFAULT 1.0,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_aStar(_pgr_get_statement($1), $2::BIGINT[],  $3::BIGINT[], $4, $5, $6::FLOAT, $7::FLOAT) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


--v3.2
CREATE FUNCTION pgr_aStar(
    TEXT,       -- edges sql (required)
    TEXT,       -- combinations_sql (required)

    directed BOOLEAN DEFAULT true,
    heuristic INTEGER DEFAULT 5,
    factor FLOAT DEFAULT 1.0,
    epsilon FLOAT DEFAULT 1.0,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_aStar(_pgr_get_statement($1), _pgr_get_statement($2), $3, $4, $5::FLOAT, $6::FLOAT) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- COMMENTS


COMMENT ON FUNCTION pgr_aStar(TEXT, BIGINT, BIGINT, BOOLEAN, INTEGER, FLOAT, FLOAT)
IS 'pgr_aStar(One to One)
- Parameters:
  - edges SQL with columns: id, source, target, cost [,reverse_cost], x1, y1, x2, y2
  - From vertex identifier
  - To vertex identifier
- Optional Parameters:
  - directed := true
  - heuristic := 5
  - factor := 1
  - epsilon := 1
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_aStar.html
';


COMMENT ON FUNCTION pgr_aStar(TEXT, BIGINT, ANYARRAY, BOOLEAN, INTEGER, FLOAT, FLOAT)
IS 'pgr_aStar(One to Many)
- Parameters:
  - edges SQL with columns: id, source, target, cost [,reverse_cost], x1, y1, x2, y2
  - From vertex identifier
  - To ARRAY[vertices identifiers]
- Optional Parameters:
  - directed := true
  - heuristic := 5
  - factor := 1
  - epsilon := 1
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_aStar.html
';


COMMENT ON FUNCTION pgr_aStar(TEXT, ANYARRAY, BIGINT, BOOLEAN, INTEGER, FLOAT, FLOAT)
IS 'pgr_aStar(Many to One)
- Parameters:
  - edges SQL with columns: id, source, target, cost [,reverse_cost], x1, y1, x2, y2
  - From ARRAY[vertices identifiers]
  - To vertex identifier
- Optional Parameters:
  - directed := true
  - heuristic := 5
  - factor := 1
  - epsilon := 1
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_aStar.html
';


  COMMENT ON FUNCTION pgr_aStar(TEXT, ANYARRAY, ANYARRAY, BOOLEAN, INTEGER, FLOAT, FLOAT)
IS 'pgr_aStar(Many to Many)
 - Parameters:
   - edges SQL with columns: id, source, target, cost [,reverse_cost], x1, y1, x2, y2
   - From ARRAY[vertices identifiers]
   - To ARRAY[vertices identifiers]
 - Optional Parameters:
   - directed := true
   - heuristic := 5
   - factor := 1
   - epsilon := 1
 - Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_aStar.html
';


COMMENT ON FUNCTION pgr_aStar(TEXT, TEXT, BOOLEAN, INTEGER, FLOAT, FLOAT)
IS 'pgr_aStar(Combinations)
 - Parameters:
   - Edges SQL with columns: id, source, target, cost [,reverse_cost], x1, y1, x2, y2
   - Combinations SQL with columns: source, target
 - Optional Parameters:
   - directed := true
   - heuristic := 5
   - factor := 1
   - epsilon := 1
 - Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_aStar.html
';



-----------------
-- pgr_aStarCost
-----------------


--v2.6
CREATE FUNCTION pgr_aStarCost(
    TEXT,     -- edges sql (required)
    BIGINT,   -- from_vid (required)
    BIGINT,   -- to_vid (required)

    directed BOOLEAN DEFAULT true,
    heuristic INTEGER DEFAULT 5,
    factor FLOAT DEFAULT 1.0,
    epsilon FLOAT DEFAULT 1.0,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
    SELECT a.start_vid, a.end_vid, a.agg_cost
    FROM _pgr_aStar(_pgr_get_statement($1), ARRAY[$2]::BIGINT[],  ARRAY[$3]::BIGINT[], $4, $5, $6::FLOAT, $7::FLOAT, true) AS a
    ORDER BY  a.start_vid, a.end_vid;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;



--v2.6
CREATE FUNCTION pgr_aStarCost(
    TEXT,       -- edges sql (required)
    BIGINT,     -- from_vid (required)
    ANYARRAY,   -- to_vids (required)

    directed BOOLEAN DEFAULT true,
    heuristic INTEGER DEFAULT 5,
    factor FLOAT DEFAULT 1.0,
    epsilon FLOAT DEFAULT 1.0,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.start_vid, a.end_vid, a.agg_cost
    FROM _pgr_aStar(_pgr_get_statement($1), ARRAY[$2]::BIGINT[],  $3::BIGINT[], $4, $5, $6::FLOAT, $7::FLOAT, true) AS a
    ORDER BY  a.start_vid, a.end_vid;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;



--v2.6
CREATE FUNCTION pgr_aStarCost(
    TEXT,       -- edges sql (required)
    ANYARRAY,   -- from_vids (required)
    BIGINT,     -- to_vid (required)

    directed BOOLEAN DEFAULT true,
    heuristic INTEGER DEFAULT 5,
    factor FLOAT DEFAULT 1.0,
    epsilon FLOAT DEFAULT 1.0,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.start_vid, a.end_vid, a.agg_cost
    FROM _pgr_aStar(_pgr_get_statement($1), $2::BIGINT[],  ARRAY[$3]::BIGINT[], $4, $5, $6::FLOAT, $7::FLOAT, true, normal:=false) AS a
    ORDER BY  a.start_vid, a.end_vid;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


--v2.6
CREATE FUNCTION pgr_aStarCost(
    TEXT,       -- edges sql (required)
    ANYARRAY,   -- from_vids (required)
    ANYARRAY,   -- to_vids (required)

    directed BOOLEAN DEFAULT true,
    heuristic INTEGER DEFAULT 5,
    factor FLOAT DEFAULT 1.0,
    epsilon FLOAT DEFAULT 1.0,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
    SELECT a.start_vid, a.end_vid, a.agg_cost
    FROM _pgr_aStar(_pgr_get_statement($1), $2::BIGINT[],  $3::BIGINT[], $4, $5, $6::FLOAT, $7::FLOAT, true) AS a
    ORDER BY  a.start_vid, a.end_vid;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


--v3.2
CREATE FUNCTION pgr_aStarCost(
    TEXT,       -- edges sql (required)
    TEXT,       -- combinations_sql (required)

    directed BOOLEAN DEFAULT true,
    heuristic INTEGER DEFAULT 5,
    factor FLOAT DEFAULT 1.0,
    epsilon FLOAT DEFAULT 1.0,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
    SELECT a.start_vid, a.end_vid, a.agg_cost
    FROM _pgr_aStar(_pgr_get_statement($1), _pgr_get_statement($2), $3, $4, $5::FLOAT, $6::FLOAT, true) AS a
    ORDER BY  a.start_vid, a.end_vid;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- COMMENTS

COMMENT ON FUNCTION pgr_aStarCost(TEXT, BIGINT, BIGINT, BOOLEAN, INTEGER, FLOAT, FLOAT)
IS 'pgr_aStarCost(One to One)
- Parameters:
  - edges SQL with columns: id, source, target, cost [,reverse_cost], x1, y1, x2, y2
  - From vertex identifier
  - To vertex identifier
- Optional Parameters:
  - directed := true
  - heuristic := 5
  - factor := 1
  - epsilon := 1
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_aStarCost.html
';


COMMENT ON FUNCTION pgr_aStarCost(TEXT, BIGINT, ANYARRAY, BOOLEAN, INTEGER, FLOAT, FLOAT)
IS 'pgr_aStarCost(One to Many)
- Parameters:
  - edges SQL with columns: id, source, target, cost [,reverse_cost], x1, y1, x2, y2
  - From vertex identifier
  - To ARRAY[vertices identifiers]
- Optional Parameters:
  - directed := true
  - heuristic := 5
  - factor := 1
  - epsilon := 1
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_aStarCost.html
';


COMMENT ON FUNCTION pgr_aStarCost(TEXT, ANYARRAY, BIGINT, BOOLEAN, INTEGER, FLOAT, FLOAT)
IS 'pgr_aStarCost(Many to One)
- Parameters:
  - edges SQL with columns: id, source, target, cost [,reverse_cost], x1, y1, x2, y2
  - From ARRAY[vertices identifiers]
  - To vertex identifier
- Optional Parameters:
  - directed := true
  - heuristic := 5
  - factor := 1
  - epsilon := 1
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_aStarCost.html
';


COMMENT ON FUNCTION pgr_aStarCost(TEXT, ANYARRAY, ANYARRAY, BOOLEAN, INTEGER, FLOAT, FLOAT)
IS 'pgr_aStarCost(Many to Many)
 - Parameters:
   - edges SQL with columns: id, source, target, cost [,reverse_cost], x1, y1, x2, y2
   - From ARRAY[vertices identifiers]
   - To ARRAY[vertices identifiers]
 - Optional Parameters:
   - directed := true
   - heuristic := 5
   - factor := 1
   - epsilon := 1
 - Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_aStarCost.html
';


COMMENT ON FUNCTION pgr_aStarCost(TEXT, TEXT, BOOLEAN, INTEGER, FLOAT, FLOAT)
IS 'pgr_aStarCost(Combinations)
 - Parameters:
   - edges SQL with columns: id, source, target, cost [,reverse_cost], x1, y1, x2, y2
   - Combinations SQL with columns: source, target
 - Optional Parameters:
   - directed := true
   - heuristic := 5
   - factor := 1
   - epsilon := 1
 - Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_aStarCost.html
';


-----------------------------
-- pgr_aStarCostMatrix
-----------------------------


--v2.6
CREATE FUNCTION pgr_aStarCostMatrix(
    TEXT,     -- edges sql (required)
    ANYARRAY, -- vids (required)

    directed BOOLEAN DEFAULT true,
    heuristic INTEGER DEFAULT 5,
    factor FLOAT DEFAULT 1.0,
    epsilon FLOAT DEFAULT 1.0,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
    SELECT a.start_vid, a.end_vid, a.agg_cost
    FROM _pgr_astar(_pgr_get_statement($1), $2, $2, $3, $4, $5::FLOAT, $6::FLOAT, true) a;
$BODY$
LANGUAGE SQL VOLATILE
COST 100
ROWS 1000;

-- COMMENT

COMMENT ON FUNCTION pgr_aStarCostMatrix(TEXT, ANYARRAY, BOOLEAN, INTEGER, FLOAT, FLOAT)
IS 'pgr_aStarCostMatrix
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost], x1, y1, x2, y2
    - ARRAY [vertices identifiers]
- Optional Parameters:
    - directed := true
    - heuristic := 5
    - factor := 1
    - epsilon := 1
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_aStarCostMatrix.html
';


-------------------
-- pgr_withPointsDD
-------------------

--v3.0
CREATE FUNCTION _pgr_withPointsDD(
    edges_sql TEXT,
    points_sql TEXT,
    start_pid ANYARRAY,
    distance FLOAT,

    directed BOOLEAN DEFAULT true,
    driving_side CHAR DEFAULT 'b',
    details BOOLEAN DEFAULT false,
    equicost BOOLEAN DEFAULT false,

    OUT seq INTEGER,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE C VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_withPointsDD(TEXT, TEXT, ANYARRAY, FLOAT, BOOLEAN, CHAR, BOOLEAN, BOOLEAN)
IS 'pgRouting internal function';


-- SINGLE
--v2.6
CREATE FUNCTION pgr_withPointsDD(
    TEXT,   --edges_sql (required)
    TEXT,   -- points_sql (required)
    BIGINT, -- from_vid (required)
    FLOAT,  -- distance (required)

    directed BOOLEAN DEFAULT true,
    driving_side CHAR DEFAULT 'b',
    details BOOLEAN DEFAULT false,

    OUT seq INTEGER,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT seq, node, edge, cost, agg_cost
    FROM _pgr_withPointsDD(_pgr_get_statement($1), _pgr_get_statement($2), ARRAY[$3]::BIGINT[], $4, $5, $6, $7, false);
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;

-- MULTIPLE
--v2.6
CREATE FUNCTION pgr_withPointsDD(
    TEXT,     --edges_sql (required)
    TEXT,     -- points_sql (required)
    ANYARRAY, -- from_vid (required)
    FLOAT,    -- distance (required)

    directed BOOLEAN DEFAULT true,
    driving_side CHAR DEFAULT 'b',
    details BOOLEAN DEFAULT false,
    equicost BOOLEAN DEFAULT false,

    OUT seq INTEGER,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_withPointsDD(_pgr_get_statement($1), _pgr_get_statement($2), $3, $4, $5, $6, $7, $8);
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;


-- COMMENTS

COMMENT ON FUNCTION pgr_withPointsDD(TEXT, TEXT, BIGINT, FLOAT, BOOLEAN, CHAR, BOOLEAN)
IS 'pgr_withPointsDD(Single Vertex)
- PROPOSED
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - Points SQL with columns: [pid], edge_id, fraction[,side]
    - From vertex identifier
    - Distance
- Optional Parameters
    - directed := true
    - driving_side := b
    - details := false
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_withPointsDD.html
';


COMMENT ON FUNCTION pgr_withPointsDD(TEXT, TEXT, ANYARRAY, FLOAT, BOOLEAN, CHAR, BOOLEAN, BOOLEAN)
IS 'pgr_withPointsDD(Multiple Vertices)
- PROPOSED
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - Points SQL with columns: [pid], edge_id, fraction[,side]
    - From ARRAY[vertices identifiers]
    - Distance
- Optional Parameters
    - directed := true
    - driving_side := b
    - details := false
    - equicost := false
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_withPointsDD.html
';


----------------------
-- pgr_drivingDistance
----------------------

--v3.0
CREATE FUNCTION _pgr_drivingDistance(
    edges_sql TEXT,
    start_vids ANYARRAY,
    distance FLOAT,
    directed BOOLEAN DEFAULT TRUE,
    equicost BOOLEAN DEFAULT FALSE,
    OUT seq INTEGER,
    OUT from_v  BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_drivingDistance(TEXT, ANYARRAY, FLOAT, BOOLEAN, BOOLEAN)
IS 'pgRouting internal function';


-- MULTIPLE
--v2.6
CREATE FUNCTION pgr_drivingDistance(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    FLOAT,    -- distance (required)

    directed BOOLEAN DEFAULT TRUE,
    equicost BOOLEAN DEFAULT FALSE,

    OUT seq INTEGER,
    OUT from_v  BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_drivingDistance(_pgr_get_statement($1), $2, $3, $4, $5);
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;


-- SINGLE
--v3.0
CREATE FUNCTION pgr_drivingDistance(
    TEXT,   -- edges_sql (required)
    BIGINT, -- from_vid (requierd)
    FLOAT,  -- distance (required)

    directed BOOLEAN DEFAULT TRUE,

    OUT seq INTEGER,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT seq, node, edge, cost, agg_cost
    FROM _pgr_drivingDistance(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], $3, $4, false);
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;

-- COMMENTS

COMMENT ON FUNCTION pgr_drivingDistance(TEXT, BIGINT, FLOAT, BOOLEAN)
IS 'pgr_drivingDistance(Single_vertex)
- Parameters:
   - Edges SQL with columns: id, source, target, cost [,reverse_cost]
   - From vertex identifier
   - Distance from vertex identifier
- Optional Parameters
   - directed := true
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_drivingDistance.html
';

COMMENT ON FUNCTION pgr_drivingDistance(TEXT, ANYARRAY, FLOAT, BOOLEAN, BOOLEAN)
IS 'pgr_drivingDistance(Multiple vertices)
- Parameters:
   - Edges SQL with columns: id, source, target, cost [,reverse_cost]
   - From ARRAY[vertices identifiers]
   - Distance from vertices identifiers
- Optional Parameters
   - directed := true
   - equicost := false
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_drivingDistance.html
';

---------------
---------------
-- pgr_ksp
---------------
---------------

--v2.6
CREATE FUNCTION _pgr_ksp(
    edges_sql TEXT,
    start_vid BIGINT,
    end_vid BIGINT,
    k INTEGER,

    directed BOOLEAN,
    heap_paths BOOLEAN,

    OUT seq INTEGER,
    OUT path_id INTEGER,
    OUT path_seq INTEGER,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE C VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_ksp(TEXT, BIGINT, BIGINT, INTEGER, BOOLEAN, BOOLEAN)
IS 'pgRouting internal function';


--v2.6
CREATE FUNCTION pgr_ksp(
    TEXT, -- edges_sql (required)
    BIGINT, -- from_vids (required)
    BIGINT,   -- to_vids (required)
    INTEGER, -- K (required)

    directed BOOLEAN DEFAULT true,
    heap_paths BOOLEAN DEFAULT false,

    OUT seq INTEGER,
    OUT path_id INTEGER,
    OUT path_seq INTEGER,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_ksp(_pgr_get_statement($1), $2, $3, $4, $5, $6);
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;

-- COMMENTS

COMMENT ON FUNCTION pgr_ksp(TEXT, BIGINT, BIGINT, INTEGER, BOOLEAN, BOOLEAN)
IS 'pgr_KSP
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - From vertex identifier
    - To vertex identifier
    - K
- Optional Parameters
    - directed := true
    - heap_paths := false
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_KSP.html
';


--------------------
-- pgr_withPointsKSP
--------------------

--v3.0
CREATE FUNCTION _pgr_withPointsKSP(
    edges_sql TEXT,
    points_sql TEXT,
    start_pid BIGINT,
    end_pid BIGINT,
    k INTEGER,

    directed BOOLEAN,
    heap_paths BOOLEAN,
    driving_side CHAR,
    details BOOLEAN,

    OUT seq INTEGER, OUT path_id INTEGER, OUT path_seq INTEGER,
    OUT node BIGINT, OUT edge BIGINT,
    OUT cost FLOAT, OUT agg_cost FLOAT)
  RETURNS SETOF RECORD AS
    'MODULE_PATHNAME'
    LANGUAGE c STABLE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_withPointsKSP(TEXT, TEXT, BIGINT, BIGINT, INTEGER, BOOLEAN, BOOLEAN, CHAR, BOOLEAN)
IS 'pgRouting internal function';


--v2.6
CREATE FUNCTION pgr_withPointsKSP(
    TEXT,    -- edges_sql (required)
    TEXT,    -- points_sql (required)
    BIGINT,  -- from_vid (required)
    BIGINT,  -- to_vid (required)
    INTEGER, -- K (required)

    directed BOOLEAN DEFAULT true,
    heap_paths BOOLEAN DEFAULT false,
    driving_side CHAR DEFAULT 'b',
    details BOOLEAN DEFAULT false,

    OUT seq INTEGER, OUT path_id INTEGER, OUT path_seq INTEGER,
    OUT node BIGINT, OUT edge BIGINT,
    OUT cost FLOAT, OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_withPointsKSP(_pgr_get_statement($1), _pgr_get_statement($2), $3, $4, $5, $6, $7, $8, $9);
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;

-- COMMENTS

COMMENT ON FUNCTION pgr_withPointsKSP(TEXT, TEXT, BIGINT, BIGINT, INTEGER, BOOLEAN, BOOLEAN, CHAR, BOOLEAN)
IS 'pgr_withPointsKSP
- PROPOSED
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - Points SQL with columns: [pid], edge_id, fraction[,side]
    - From vertex identifier
    - To vertex identifier
    - K
- Optional Parameters
    - directed := true
    - heap paths := false
    - driving side := b
    - details := false
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_withPointsKSP.html
';


--v3.0
CREATE FUNCTION _pgr_turnRestrictedPath(
    TEXT,   -- edges_sql
    TEXT,   -- restrictions_sql
    BIGINT, -- start_vertex
    BIGINT, -- end_vertex
    INTEGER,-- K cycles

    directed BOOLEAN,
    heap_paths BOOLEAN,
    stop_on_first BOOLEAN,
    strict BOOLEAN,

    OUT seq INTEGER,
    OUT path_id INTEGER,
    OUT path_seq INTEGER,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c IMMUTABLE STRICT;


-- COMMENTS

COMMENT ON FUNCTION _pgr_turnRestrictedPath(TEXT, TEXT, BIGINT, BIGINT, INTEGER, BOOLEAN, BOOLEAN, BOOLEAN, BOOLEAN)
IS 'pgRouting internal function';


--v3.0
CREATE FUNCTION pgr_turnRestrictedPath(
    TEXT,   -- edges_sql (required)
    TEXT,   -- restrictions_sql (required)
    BIGINT, -- start_vertex (required)
    BIGINT, -- end_vertex (required)
    INTEGER,-- K cycles (required)

    directed BOOLEAN DEFAULT true,
    heap_paths BOOLEAN DEFAULT false,
    stop_on_first BOOLEAN DEFAULT true,
    strict BOOLEAN DEFAULT false,

    OUT seq INTEGER,
    OUT path_id INTEGER,
    OUT path_seq INTEGER,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_turnRestrictedPath(_pgr_get_statement($1), _pgr_get_statement($2), $3, $4, $5, $6, $7, $8, $9);
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;


-- COMMENTS

COMMENT ON FUNCTION pgr_turnRestrictedPath(TEXT, TEXT, BIGINT, BIGINT, INTEGER, BOOLEAN, BOOLEAN, BOOLEAN, BOOLEAN)
IS 'pgr_turnRestrictedPath
- EXPERIMENTAL
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - Restrictions SQL with columns: id, cost, path
    - From vertex identifier
    - To vertex identifier
    - K
- Optional Parameters
    - directed := true
    - heap paths := false
    - stop on first := true
    - strict := false
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_turnRestrictedPath.html
';



-------------
-- tsp
-------------


--v3.0
CREATE FUNCTION _pgr_TSP(
    matrix_row_sql TEXT,
    start_id BIGINT DEFAULT 0,
    end_id BIGINT DEFAULT 0,

    max_processing_time FLOAT DEFAULT '+infinity'::FLOAT,

    tries_per_temperature INTEGER DEFAULT 500,
    max_changes_per_temperature INTEGER DEFAULT 60,
    max_consecutive_non_changes INTEGER DEFAULT 100,

    initial_temperature FLOAT DEFAULT 100,
    final_temperature FLOAT DEFAULT 0.1,
    cooling_factor FLOAT DEFAULT 0.9,

    randomize BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT node BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF record
AS 'MODULE_PATHNAME'
LANGUAGE c VOLATILE STRICT;


-- COMMENTS

COMMENT ON FUNCTION _pgr_TSP(TEXT, BIGINT, BIGINT, FLOAT, INTEGER, INTEGER, INTEGER, FLOAT, FLOAT, FLOAT, BOOLEAN)
IS 'pgRouting internal function';



--v3.0
CREATE FUNCTION _pgr_TSPeuclidean(
    coordinates_sql TEXT,
    start_id BIGINT DEFAULT 0,
    end_id BIGINT DEFAULT 0,

    max_processing_time FLOAT DEFAULT '+infinity'::FLOAT,

    tries_per_temperature INTEGER DEFAULT 500,
    max_changes_per_temperature INTEGER DEFAULT 60,
    max_consecutive_non_changes INTEGER DEFAULT 100,

    initial_temperature FLOAT DEFAULT 100,
    final_temperature FLOAT DEFAULT 0.1,
    cooling_factor FLOAT DEFAULT 0.9,

    randomize BOOLEAN DEFAULT true,

    OUT seq integer,
    OUT node BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF record
AS 'MODULE_PATHNAME'
LANGUAGE C VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_TSPeuclidean(TEXT, BIGINT, BIGINT, FLOAT, INTEGER, INTEGER, INTEGER, FLOAT, FLOAT, FLOAT, BOOLEAN)
IS 'pgRouting internal function';


-------------
-- pgr_TSP
-------------


--v2.6
CREATE FUNCTION pgr_TSP(
    TEXT, -- matrix_row_sql (required)

    start_id BIGINT DEFAULT 0,
    end_id BIGINT DEFAULT 0,

    max_processing_time FLOAT DEFAULT '+infinity'::FLOAT,

    tries_per_temperature INTEGER DEFAULT 500,
    max_changes_per_temperature INTEGER DEFAULT 60,
    max_consecutive_non_changes INTEGER DEFAULT 100,

    initial_temperature FLOAT DEFAULT 100,
    final_temperature FLOAT DEFAULT 0.1,
    cooling_factor FLOAT DEFAULT 0.9,

    randomize BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT node BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_TSP(_pgr_get_statement($1), $2, $3, $4, $5, $6, $7, $8, $9, $10, $11);
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;


-- COMMENTS


COMMENT ON FUNCTION pgr_TSP(TEXT, BIGINT, BIGINT, FLOAT, INTEGER, INTEGER, INTEGER, FLOAT, FLOAT, FLOAT, BOOLEAN)
IS 'pgr_TSP
- Parameters
   - matrix SQL with columns: start_vid, end_vid, agg_cost
- Optional parameters
    - start_id := 0
    - end_id := 0

    - max_processing_time := ''+infinity''::FLOAT

    - tries_per_temperature := 500
    - max_changes_per_temperature :=  60
    - max_consecutive_non_changes :=  100

    - initial_temperature FLOAT := 100
    - final_temperature := 0.1
    - cooling_factor := 0.9

    - randomize := true
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_TSP.html
';



--------------------
-- pgr_TSPeuclidean
--------------------


--v3.0
CREATE FUNCTION pgr_TSPeuclidean(
    TEXT, -- coordinates_sql (required)

    start_id BIGINT DEFAULT 0,
    end_id BIGINT DEFAULT 0,

    max_processing_time FLOAT DEFAULT '+infinity'::FLOAT,

    tries_per_temperature INTEGER DEFAULT 500,
    max_changes_per_temperature INTEGER DEFAULT 60,
    max_consecutive_non_changes INTEGER DEFAULT 100,

    initial_temperature FLOAT DEFAULT 100,
    final_temperature FLOAT DEFAULT 0.1,
    cooling_factor FLOAT DEFAULT 0.9,

    randomize BOOLEAN DEFAULT true,

    OUT seq integer,
    OUT node BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_TSPeuclidean(_pgr_get_statement($1), $2,$3, $4, $5,$6,$7, $8,$9,$10, $11);
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;


-- COMMENTS

COMMENT ON FUNCTION pgr_TSPeuclidean(TEXT, BIGINT, BIGINT, FLOAT, INTEGER, INTEGER, INTEGER, FLOAT, FLOAT, FLOAT, BOOLEAN)
IS 'pgr_TSPeuclidean
- Parameters
   - coordinates SQL with columns: id, x, y
- Optional parameters
    - start_id := 0
    - end_id := 0

    - max_processing_time := ''+infinity''::FLOAT

    - tries_per_temperature := 500
    - max_changes_per_temperature :=  60
    - max_consecutive_non_changes :=  100

    - initial_temperature FLOAT := 100
    - final_temperature := 0.1
    - cooling_factor := 0.9

    - randomize := true
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_TSPeuclidean.html
';


--------------
--------------
-- alpha_shape
--------------
--------------

--v3.0
CREATE FUNCTION _pgr_alphaShape(
    TEXT, -- edges sql
    alpha FLOAT DEFAULT 0,

    OUT seq1 BIGINT,
    OUT textgeom TEXT)
RETURNS SETOF record
AS 'MODULE_PATHNAME'
LANGUAGE c VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_alphashape(TEXT, FLOAT)
IS 'pgrouting internal function';

--------------
--------------
-- alpha_shape
--------------
--------------


--v3.0
CREATE FUNCTION pgr_alphaShape(
    geometry, -- geometry
    alpha FLOAT DEFAULT 0
)
RETURNS geometry AS
$BODY$

DECLARE
geom      geometry;
delauny_query   TEXT;

BEGIN
    delauny_query = format($$
        WITH
        original AS (
            SELECT %1$L::geometry AS geom
        ),
        delauny AS (
            SELECT (ST_Dump(ST_DelaunayTriangles(geom, 0 , 0))).*
                FROM original
        ),
        delauny_info AS (
            SELECT delauny.path[1] AS id,
            (ST_DumpPoints(delauny.geom)).path[2] as seq,
            (ST_DumpPoints(delauny.geom)).geom
            FROM delauny
        )
        SELECT
            id,
            seq AS source,
            -1  AS target,
            1 AS cost,
            ST_X(geom)::FLOAT AS x1,
            ST_Y(geom)::FLOAT AS y1,
            0::FLOAT AS x2,
            0::FLOAT AS y2
        FROM delauny_info WHERE seq != 4;
        $$, $1);

    --RAISE NOTICE '%', delauny_query;
    -- RETURN;

    WITH a AS (SELECT 'GEOMETRYCOLLECTION(' || string_agg(textgeom,',') || ')' as geome
        FROM _pgr_alphaShape(delauny_query, $2))
    SELECT ST_GeomFromText(geome) FROM a
    INTO geom;
    RETURN geom;

END

$BODY$
LANGUAGE plpgsql VOLATILE STRICT
COST 100;

COMMENT ON FUNCTION pgr_alphashape(geometry, FLOAT)
IS 'pgr_alphaShape
- Parameters
	- An SQL with columns: geom
- Optional Parameters
	- alpha := 0
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_alphaShape.html
';


----------------
----------------
-- bdAstar
----------------
----------------



--v2.6
CREATE FUNCTION _pgr_bdAstar(
    TEXT,
    ANYARRAY,
    ANYARRAY,
    directed BOOLEAN DEFAULT true,
    heuristic INTEGER DEFAULT 5,
    factor FLOAT DEFAULT 1.0,
    epsilon FLOAT DEFAULT 1.0,
    only_cost BOOLEAN DEFAULT false,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
    'MODULE_PATHNAME'
LANGUAGE C VOLATILE STRICT;


--v3.2
CREATE FUNCTION _pgr_bdAstar(
    TEXT,
    TEXT,
    directed BOOLEAN DEFAULT true,
    heuristic INTEGER DEFAULT 5,
    factor FLOAT DEFAULT 1.0,
    epsilon FLOAT DEFAULT 1.0,
    only_cost BOOLEAN DEFAULT false,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
    'MODULE_PATHNAME'
LANGUAGE C VOLATILE STRICT;


-- COMMENTS

COMMENT ON FUNCTION _pgr_bdAstar(TEXT, ANYARRAY, ANYARRAY, BOOLEAN, INTEGER, FLOAT, FLOAT, BOOLEAN)
IS 'pgRouting internal function';

COMMENT ON FUNCTION _pgr_bdAstar(TEXT, TEXT, BOOLEAN, INTEGER, FLOAT, FLOAT, BOOLEAN)
IS 'pgRouting internal function';

--------------------
-- pgr_bdAstar
--------------------


-- one to one
--v2.6
CREATE FUNCTION pgr_bdAstar(
    TEXT,   -- edges_sql (required)
    BIGINT, -- from_vid (required)
    BIGINT, -- to_vid (required)

    directed BOOLEAN DEFAULT true,
    heuristic INTEGER DEFAULT 5,
    factor NUMERIC DEFAULT 1.0,
    epsilon NUMERIC DEFAULT 1.0,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_bdAstar(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], ARRAY[$3]::BIGINT[], $4, $5, $6::FLOAT, $7::FLOAT, false) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;

-- one to many
--v2.6
CREATE FUNCTION pgr_bdAstar(
    TEXT,     -- edges_sql (required)
    BIGINT,   -- from_vid (required)
    ANYARRAY, -- to_vids (required)

    directed BOOLEAN DEFAULT true,
    heuristic INTEGER DEFAULT 5,
    factor NUMERIC DEFAULT 1.0,
    epsilon NUMERIC DEFAULT 1.0,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.end_vid, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_bdAstar(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], $3::BIGINT[], $4, $5, $6::FLOAT, $7::FLOAT, false) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;

-- many to one
--v2.6
CREATE FUNCTION pgr_bdAstar(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    BIGINT,   -- to_vid (required)

    directed BOOLEAN DEFAULT true,
    heuristic INTEGER DEFAULT 5,
    factor NUMERIC DEFAULT 1.0,
    epsilon NUMERIC DEFAULT 1.0,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.start_vid, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_bdAstar(_pgr_get_statement($1), $2::BIGINT[], ARRAY[$3]::BIGINT[], $4, $5, $6::FLOAT, $7::FLOAT, false) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;

-- many to many
--v2.6
CREATE FUNCTION pgr_bdAstar(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    ANYARRAY, -- to_vids (required)

    directed BOOLEAN DEFAULT true,
    heuristic INTEGER DEFAULT 5,
    factor NUMERIC DEFAULT 1.0,
    epsilon NUMERIC DEFAULT 1.0,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_bdAstar(_pgr_get_statement($1), $2::BIGINT[], $3::BIGINT[], $4, $5, $6::FLOAT, $7::FLOAT, false);
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;

-- combinations
--v3.2
CREATE FUNCTION pgr_bdAstar(
    TEXT,     -- edges_sql (required)
    TEXT,     -- combinations_sql (required)

    directed BOOLEAN DEFAULT true,
    heuristic INTEGER DEFAULT 5,
    factor NUMERIC DEFAULT 1.0,
    epsilon NUMERIC DEFAULT 1.0,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_bdAstar(_pgr_get_statement($1), _pgr_get_statement($2), $3, $4, $5::FLOAT, $6::FLOAT, false);
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;

-- COMMENTS

COMMENT ON FUNCTION pgr_bdAstar(TEXT, BIGINT, BIGINT, BOOLEAN, INTEGER, NUMERIC, NUMERIC)
IS 'pgr_bdAstar(One to One)
- Parameters:
  - edges SQL with columns: id, source, target, cost [,reverse_cost], x1, y1, x2, y2
  - From vertex identifier
  - To vertex identifier
- Optional Parameters:
  - directed := true
  - heuristic := 5
  - factor := 1
  - epsilon := 1
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_bdAstar.html
';

COMMENT ON FUNCTION pgr_bdAstar(TEXT, BIGINT, ANYARRAY, BOOLEAN, INTEGER, NUMERIC, NUMERIC)
IS 'pgr_bdAstar(One to Many)
- Parameters:
  - edges SQL with columns: id, source, target, cost [,reverse_cost], x1, y1, x2, y2
  - From vertex identifier
  - To ARRAY[vertices identifiers]
- Optional Parameters:
  - directed := true
  - heuristic := 5
  - factor := 1
  - epsilon := 1
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_bdAstar.html
';


COMMENT ON FUNCTION pgr_bdAstar(TEXT, ANYARRAY, BIGINT, BOOLEAN, INTEGER, NUMERIC, NUMERIC)
IS 'pgr_bdAstar(Many to One)
- Parameters:
  - edges SQL with columns: id, source, target, cost [,reverse_cost], x1, y1, x2, y2
  - From ARRAY[vertices identifiers]
  - To vertex identifier
- Optional Parameters:
  - directed := true
  - heuristic := 5
  - factor := 1
  - epsilon := 1
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_bdAstar.html
';

COMMENT ON FUNCTION pgr_bdAstar(TEXT, ANYARRAY, ANYARRAY, BOOLEAN, INTEGER, NUMERIC, NUMERIC)
IS 'pgr_bdAstar(Many to Many)
- Parameters:
  - edges SQL with columns: id, source, target, cost [,reverse_cost], x1, y1, x2, y2
  - From ARRAY[vertices identifiers]
  - To ARRAY[vertices identifiers]
- Optional Parameters:
  - directed := true
  - heuristic := 5
  - factor := 1
  - epsilon := 1
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_bdAstar.html
';

COMMENT ON FUNCTION pgr_bdAstar(TEXT, TEXT, BOOLEAN, INTEGER, NUMERIC, NUMERIC)
IS 'pgr_bdAstar(Combinations)
- Parameters:
  - Edges SQL with columns: id, source, target, cost [, reverse_cost], x1, y1, x2, y2
  - Combinations SQL with columns: source, target
- Optional Parameters:
  - directed := true
  - heuristic := 5
  - factor := 1
  - epsilon := 1
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_bdAstar.html
';


--------------------
-- pgr_bdAstarCost
--------------------

-- one to one
--v2.6
CREATE FUNCTION pgr_bdAstarCost(
    TEXT,   -- edges_sql (required)
    BIGINT, -- from_vid (required)
    BIGINT, -- to_vid (required)

    directed BOOLEAN DEFAULT true,
    heuristic INTEGER DEFAULT 5,
    factor NUMERIC DEFAULT 1.0,
    epsilon NUMERIC DEFAULT 1.0,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.start_vid, a.end_vid, a.agg_cost
    FROM _pgr_bdAstar(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], ARRAY[$3]::BIGINT[], $4, $5, $6::FLOAT, $7::FLOAT, true) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- one to many
--v2.6
CREATE FUNCTION pgr_bdAstarCost(
    TEXT,     -- edges_sql (required)
    BIGINT,   -- from_vid (required)
    ANYARRAY, -- to_vidd (required)

    directed BOOLEAN DEFAULT true,
    heuristic INTEGER DEFAULT 5,
    factor NUMERIC DEFAULT 1.0,
    epsilon NUMERIC DEFAULT 1.0,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.start_vid, a.end_vid, a.agg_cost
    FROM _pgr_bdAstar(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], $3::BIGINT[], $4, $5, $6::FLOAT, $7::FLOAT, true) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- many to one
--v2.6
CREATE FUNCTION pgr_bdAstarCost(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    BIGINT,   -- to_vid (required)

    directed BOOLEAN DEFAULT true,
    heuristic INTEGER DEFAULT 5,
    factor NUMERIC DEFAULT 1.0,
    epsilon NUMERIC DEFAULT 1.0,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.start_vid, a.end_vid, a.agg_cost
    FROM _pgr_bdAstar(_pgr_get_statement($1), $2::BIGINT[], ARRAY[$3]::BIGINT[], $4, $5, $6::FLOAT, $7::FLOAT, true) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;



-- many to many
--v2.6
CREATE FUNCTION pgr_bdAstarCost(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    ANYARRAY, -- to_vids (required)

    directed BOOLEAN DEFAULT true,
    heuristic INTEGER DEFAULT 5,
    factor NUMERIC DEFAULT 1.0,
    epsilon NUMERIC DEFAULT 1.0,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.start_vid, a.end_vid, a.agg_cost
    FROM _pgr_bdAstar(_pgr_get_statement($1), $2::BIGINT[], $3::BIGINT[], $4, $5, $6::FLOAT, $7::FLOAT, true) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- combinations
--v3.2
CREATE FUNCTION pgr_bdAstarCost(
    TEXT,     -- edges_sql (required)
    TEXT,     -- combinations_sql (required)

    directed BOOLEAN DEFAULT true,
    heuristic INTEGER DEFAULT 5,
    factor NUMERIC DEFAULT 1.0,
    epsilon NUMERIC DEFAULT 1.0,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.start_vid, a.end_vid, a.agg_cost
    FROM _pgr_bdAstar(_pgr_get_statement($1), _pgr_get_statement($2), $3, $4, $5::FLOAT, $6::FLOAT, true) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- COMMENTS

COMMENT ON FUNCTION pgr_bdAstarCost(TEXT, BIGINT, BIGINT, BOOLEAN, INTEGER, NUMERIC, NUMERIC)
IS 'pgr_bdAstarCost(One to One)
- Parameters:
  - edges SQL with columns: id, source, target, cost [,reverse_cost], x1, y1, x2, y2
  - From vertex identifier
  - To vertex identifier
- Optional Parameters:
  - directed := true
  - heuristic := 5
  - factor := 1
  - epsilon := 1
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_bdAstarCost.html
';


COMMENT ON FUNCTION pgr_bdAstarCost(TEXT, BIGINT, ANYARRAY, BOOLEAN, INTEGER, NUMERIC, NUMERIC)
IS 'pgr_bdAstarCost(One to Many)
- Parameters:
  - edges SQL with columns: id, source, target, cost [,reverse_cost], x1, y1, x2, y2
  - From vertex identifier
  - To ARRAY[vertices identifiers]
- Optional Parameters:
  - directed := true
  - heuristic := 5
  - factor := 1
  - epsilon := 1
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_bdAstarCost.html
';


COMMENT ON FUNCTION pgr_bdAstarCost(TEXT, ANYARRAY, BIGINT, BOOLEAN, INTEGER, NUMERIC, NUMERIC)
IS 'pgr_bdAstarCost(Many to One)
- Parameters:
  - edges SQL with columns: id, source, target, cost [,reverse_cost], x1, y1, x2, y2
  - From ARRAY[vertices identifiers]
  - To vertex identifier
- Optional Parameters:
  - directed := true
  - heuristic := 5
  - factor := 1
  - epsilon := 1
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_bdAstarCost.html
';


COMMENT ON FUNCTION pgr_bdAstarCost(TEXT, ANYARRAY, ANYARRAY, BOOLEAN, INTEGER, NUMERIC, NUMERIC)
IS 'pgr_bdAstarCost(Many to Many)
- Parameters:
  - edges SQL with columns: id, source, target, cost [,reverse_cost], x1, y1, x2, y2
  - From ARRAY[vertices identifiers]
  - To ARRAY[vertices identifiers]
- Optional Parameters:
  - directed := true
  - heuristic := 5
  - factor := 1
  - epsilon := 1
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_bdAstarCost.html
';


COMMENT ON FUNCTION pgr_bdAstarCost(TEXT, TEXT, BOOLEAN, INTEGER, NUMERIC, NUMERIC)
IS 'pgr_bdAstarCost(Combinations)
- Parameters:
  - Edges SQL with columns: id, source, target, cost [, reverse_cost], x1, y1, x2, y2
  - Combinations SQL with columns: source, target
- Optional Parameters:
  - directed := true
  - heuristic := 5
  - factor := 1
  - epsilon := 1
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_bdAstarCost.html
';



-----------------------------
-- pgr_bdAstarCostMatrix
-----------------------------


--v2.6
CREATE FUNCTION pgr_bdAstarCostMatrix(
    TEXT,     -- edges sql (required)
    ANYARRAY, -- vids (required)

    directed BOOLEAN DEFAULT true,
    heuristic INTEGER DEFAULT 5,
    factor NUMERIC DEFAULT 1.0,
    epsilon NUMERIC DEFAULT 1.0,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost float)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.start_vid, a.end_vid, a.agg_cost
    FROM _pgr_bdAstar(_pgr_get_statement($1), $2::BIGINT[], $2::BIGINT[], $3, $4, $5::FLOAT, $6::FLOAT, true) a;
$BODY$
LANGUAGE SQL VOLATILE
COST 100
ROWS 1000;


-- COMMENT

COMMENT ON FUNCTION pgr_bdAstarCostMatrix(TEXT, ANYARRAY, BOOLEAN, INTEGER, NUMERIC, NUMERIC)
IS 'pgr_bdAstarCostMatrix
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost], x1, y1, x2, y2
    - ARRAY [vertices identifiers]
- Optional Parameters:
    - directed := true
    - heuristic := 5
    - factor := 1
    - epsilon := 1
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_bdAstarCostMatrix.html
';


-------------
-------------
-- bdDijkstra
-------------
-------------


--v3.0
CREATE FUNCTION _pgr_bdDijkstra(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- start_vids (required)
    ANYARRAY, -- end_vids (required)

    directed BOOLEAN,
    only_cost BOOLEAN DEFAULT false,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c VOLATILE STRICT;


--v3.2
CREATE FUNCTION _pgr_bdDijkstra(
    TEXT,     -- edges_sql (required)
    TEXT,     -- combinations_sql (required)

    directed BOOLEAN,
    only_cost BOOLEAN DEFAULT false,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c VOLATILE STRICT;


-- COMMENTS

COMMENT ON FUNCTION _pgr_bdDijkstra(TEXT, ANYARRAY, ANYARRAY, BOOLEAN, BOOLEAN)
IS 'pgRouting internal function';

COMMENT ON FUNCTION _pgr_bdDijkstra(TEXT, TEXT, BOOLEAN, BOOLEAN)
IS 'pgRouting internal function';



-------------------
-- pgr_bdDijkstra
-------------------


-- ONE TO ONE
--v2.6
CREATE FUNCTION pgr_bdDijkstra(
    TEXT,   -- edges_sql (required)
    BIGINT, -- from_vid
    BIGINT, -- to_vid

    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_bdDijkstra(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], ARRAY[$3]::BIGINT[], $4, false) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- ONE TO MANY
--v2.6
CREATE FUNCTION pgr_bdDijkstra(
    TEXT,    -- edges_sql (required)
    BIGINT,   -- from_vid (required)
    ANYARRAY, -- to_vids (required)

    directed BOOLEAN DEFAULT TRUE,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.end_vid, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_bdDijkstra(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], $3::BIGINT[], $4, false) as a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- MANY TO ONE
--v2.6
CREATE FUNCTION pgr_bdDijkstra(
    TEXT,    -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    BIGINT,   -- to_vid (required)

    directed BOOLEAN DEFAULT TRUE,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.start_vid, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_bdDijkstra(_pgr_get_statement($1), $2::BIGINT[], ARRAY[$3]::BIGINT[], $4, false) as a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;



-- MANY TO MANY
--v2.6
CREATE FUNCTION pgr_bdDijkstra(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    ANYARRAY, -- to_vids (required)

    directed BOOLEAN DEFAULT TRUE,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_bdDijkstra(_pgr_get_statement($1), $2::BIGINT[], $3::BIGINT[], directed, false) as a;
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;


-- COMBINATIONS
--v3.2
CREATE FUNCTION pgr_bdDijkstra(
    TEXT,     -- edges_sql (required)
    TEXT,     -- combinations_sql (required)

    directed BOOLEAN DEFAULT TRUE,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_bdDijkstra(_pgr_get_statement($1), _pgr_get_statement($2), directed, false) as a;
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;


-- COMMENTS

COMMENT ON FUNCTION pgr_bdDijkstra(TEXT, BIGINT, BIGINT, BOOLEAN)
IS 'pgr_bdDijkstra(One to One)
- Parameters:
  - edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From vertex identifier
  - To vertex identifier
- Optional Parameters:
  - directed := true
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_bdDijkstra.html
';

COMMENT ON FUNCTION pgr_bdDijkstra(TEXT, BIGINT, ANYARRAY, BOOLEAN)
IS 'pgr_bdDijkstra(One to Many)
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From vertex identifier
  - To ARRAY[vertices identifiers]
- Optional Parameters
  - directed := true
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_bdDijkstra.html
';

COMMENT ON FUNCTION pgr_bdDijkstra(TEXT, ANYARRAY, BIGINT, BOOLEAN)
IS 'pgr_bdDijkstra(Many to One)
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From ARRAY[vertices identifiers]
  - To vertex identifier
- Optional Parameters
  - directed := true
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_bdDijkstra.html
';

COMMENT ON FUNCTION pgr_bdDijkstra(TEXT, ANYARRAY, ANYARRAY, BOOLEAN)
IS 'pgr_bdDijkstra(Many to Many)
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From ARRAY[vertices identifiers]
  - To ARRAY[vertices identifiers]
- Optional Parameters
  - directed := true
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_bdDijkstra.html
';

COMMENT ON FUNCTION pgr_bdDijkstra(TEXT, TEXT, BOOLEAN)
IS 'pgr_bdDijkstra(Combinations)
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - Combinations SQL with columns: source, target
- Optional Parameters
  - directed := true
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_bdDijkstra.html
';



----------------------
-- pgr_bdDijkstraCost
----------------------


-- ONE TO ONE
--v2.6
CREATE FUNCTION pgr_bdDijkstraCost(
    TEXT,   -- edges_sql (required)
    BIGINT, -- from_vid (required)
    BIGINT, -- to_vid (required)

    directed BOOLEAN DEFAULT true,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.start_vid, a.end_vid, a.agg_cost
    FROM _pgr_bdDijkstra(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], ARRAY[$3]::BIGINT[], $4, true) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- ONE TO MANY
--v2.6
CREATE FUNCTION pgr_bdDijkstraCost(
    TEXT,     -- edges_sql (required)
    BIGINT,   -- from_vid (required)
    ANYARRAY, -- to_vids (required)

    directed BOOLEAN DEFAULT true,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.start_vid, a.end_vid, a.agg_cost
    FROM _pgr_bdDijkstra(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], $3::BIGINT[], $4, true) as a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- MANY TO ONE
--v2.6
CREATE FUNCTION pgr_bdDijkstraCost(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    BIGINT,   -- to_vid (required)

    directed BOOLEAN DEFAULT true,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.start_vid, a.end_vid, a.agg_cost
    FROM _pgr_bdDijkstra(_pgr_get_statement($1), $2::BIGINT[], ARRAY[$3]::BIGINT[], $4, true) as a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- MANY TO MANY
--v2.6
CREATE FUNCTION pgr_bdDijkstraCost(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    ANYARRAY, -- to_vids (required)

    directed BOOLEAN DEFAULT true,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.start_vid, a.end_vid, a.agg_cost
    FROM _pgr_bdDijkstra(_pgr_get_statement($1), $2::BIGINT[], $3::BIGINT[], directed, true) as a;
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;


-- COMBINATIONS
--v3.2
CREATE FUNCTION pgr_bdDijkstraCost(
    TEXT,     -- edges_sql (required)
    TEXT,     -- combinations_sql (required)

    directed BOOLEAN DEFAULT true,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.start_vid, a.end_vid, a.agg_cost
    FROM _pgr_bdDijkstra(_pgr_get_statement($1), _pgr_get_statement($2), directed, true) as a;
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;


-- COMMENTS

COMMENT ON FUNCTION pgr_bdDijkstraCost(TEXT, BIGINT, BIGINT, BOOLEAN)
IS 'pgr_bdDijkstraCost(One to One)
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From vertex identifier
  - To vertex identifier
- Optional Parameters
  - directed := true
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_bdDijkstraCost.html
';


COMMENT ON FUNCTION pgr_bdDijkstraCost(TEXT, BIGINT, ANYARRAY, BOOLEAN)
IS 'pgr_bdDijkstraCost(One to Many)
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From vertex identifier
  - To ARRAY[vertices identifiers]
- Optional Parameters
  - directed := true
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_bdDijkstraCost.html
';


COMMENT ON FUNCTION pgr_bdDijkstraCost(TEXT, ANYARRAY, BIGINT, BOOLEAN)
IS 'pgr_bdDijkstraCost(Many to One)
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From ARRAY[vertices identifiers]
  - To vertex identifier
- Optional Parameters
  - directed := true
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_bdDijkstraCost.html
';


COMMENT ON FUNCTION pgr_bdDijkstraCost(TEXT, ANYARRAY, ANYARRAY, BOOLEAN)
IS 'pgr_bdDijkstraCost(Many to Many)
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From ARRAY[vertices identifiers]
  - To ARRAY[vertices identifiers]
- Optional Parameters
  - directed := true
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_bdDijkstraCost.html
';

COMMENT ON FUNCTION pgr_bdDijkstraCost(TEXT, TEXT, BOOLEAN)
IS 'pgr_bdDijkstraCost(Combinations)
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - Combinations SQL with columns: source, target
- Optional Parameters
  - directed := true
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_bdDijkstraCost.html
';


-----------------------------
-- pgr_bdDijkstraCostMatrix
-----------------------------

--v2.6
CREATE FUNCTION pgr_bdDijkstraCostMatrix(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- vids (required)

    directed BOOLEAN DEFAULT true,

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.start_vid, a.end_vid, a.agg_cost
    FROM _pgr_bdDijkstra(_pgr_get_statement($1), $2::BIGINT[], $2::BIGINT[], $3, true) a;
$BODY$
LANGUAGE SQL VOLATILE
COST 100
ROWS 1000;

-- COMMENT

COMMENT ON FUNCTION pgr_bdDijkstraCostMatrix(TEXT, ANYARRAY, BOOLEAN)
IS 'pgr_bdDijkstraCostMatrix
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - ARRAY [vertices identifiers]
- Optional Parameters
    - directed := true
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_bdDijkstraCostMatrix.html
';


--------------
--------------
-- trsp
--------------
--------------


--------------
-- _trsp
--------------


--v2.6
CREATE FUNCTION _trsp(
    TEXT, -- edges SQL
    TEXT, -- restrictions SQL
    ANYARRAY,
    ANYARRAY,
    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE 'c' VOLATILE;

--v3.4
CREATE FUNCTION _v4trsp(
    TEXT, -- edges SQL
    TEXT, -- restrictions SQL
    ANYARRAY,
    ANYARRAY,
    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE 'c' VOLATILE;

--v3.4
CREATE FUNCTION _v4trsp(
    TEXT, -- edges SQL
    TEXT, -- restrictions SQL
    TEXT, -- combinations SQL
    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE 'c' VOLATILE;

-- COMMENTS
COMMENT ON FUNCTION _trsp(TEXT, TEXT, ANYARRAY, ANYARRAY, BOOLEAN)
IS 'pgRouting internal function';

COMMENT ON FUNCTION _v4trsp(TEXT, TEXT, ANYARRAY, ANYARRAY, BOOLEAN)
IS 'pgRouting internal function';

COMMENT ON FUNCTION _v4trsp(TEXT, TEXT, TEXT, BOOLEAN)
IS 'pgRouting internal function';


-- ONE to ONE
--v3.4
CREATE FUNCTION pgr_trsp(
  TEXT, -- Edges SQL
  TEXT, -- Restrictions SQL
  BIGINT, -- departure
  BIGINT, -- destination
  directed BOOLEAN DEFAULT true,

  OUT seq INTEGER,
  OUT path_seq INTEGER,
  OUT start_vid BIGINT,
  OUT end_vid BIGINT,
  OUT node BIGINT,
  OUT edge BIGINT,
  OUT cost FLOAT,
  OUT agg_cost FLOAT
)
RETURNS SETOF record AS
$BODY$

  SELECT * FROM _v4trsp(
    _pgr_get_statement($1),
    _pgr_get_statement($2),
    ARRAY[$3]::BIGINT[],
    ARRAY[$4]::BIGINT[],
    directed) AS a;

$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;

-- ONE to MANY
--v3.4
CREATE FUNCTION pgr_trsp(
  TEXT, -- Edges SQL
  TEXT, -- Restrictions SQL
  BIGINT, -- departure
  ANYARRAY, -- destinations
  directed BOOLEAN DEFAULT true,

  OUT seq INTEGER,
  OUT path_seq INTEGER,
  OUT start_vid BIGINT,
  OUT end_vid BIGINT,
  OUT node BIGINT,
  OUT edge BIGINT,
  OUT cost FLOAT,
  OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
  SELECT * FROM _v4trsp(
    _pgr_get_statement($1),
    _pgr_get_statement($2),
    ARRAY[$3]::BIGINT[],
    $4::BIGINT[],
    directed) AS a;
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;

-- MANY to ONE
--v3.4
CREATE FUNCTION pgr_trsp(
  TEXT, -- Edges SQL
  TEXT, -- Restrictions SQL
  ANYARRAY, -- departures
  BIGINT, -- destination
  directed BOOLEAN DEFAULT true,

  OUT seq INTEGER,
  OUT path_seq INTEGER,
  OUT start_vid BIGINT,
  OUT end_vid BIGINT,
  OUT node BIGINT,
  OUT edge BIGINT,
  OUT cost FLOAT,
  OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
  SELECT * FROM _v4trsp(
    _pgr_get_statement($1),
    _pgr_get_statement($2),
    $3::BIGINT[],
    ARRAY[$4]::BIGINT[],
    $5) AS a;
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;

-- MANY to MANY
--v3.4
CREATE FUNCTION pgr_trsp(
  TEXT, -- Edges SQL
  TEXT, -- Restrictions SQL
  ANYARRAY, -- departures
  ANYARRAY, -- destinations
  directed BOOLEAN DEFAULT true,

  OUT seq INTEGER,
  OUT path_seq INTEGER,
  OUT start_vid BIGINT,
  OUT end_vid BIGINT,
  OUT node BIGINT,
  OUT edge BIGINT,
  OUT cost FLOAT,
  OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
  SELECT * FROM _v4trsp(
    _pgr_get_statement($1),
    _pgr_get_statement($2),
    $3::BIGINT[],
    $4::BIGINT[],
    $5) AS a;
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;

-- COMBINATIONS
--v3.4
CREATE FUNCTION pgr_trsp(
  TEXT, -- Edges SQL
  TEXT, -- Restrictions SQL
  TEXT, -- Combinations SQL
  directed BOOLEAN DEFAULT true,

  OUT seq INTEGER,
  OUT path_seq INTEGER,
  OUT start_vid BIGINT,
  OUT end_vid BIGINT,
  OUT node BIGINT,
  OUT edge BIGINT,
  OUT cost FLOAT,
  OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
  SELECT * FROM _v4trsp(
    _pgr_get_statement($1),
    _pgr_get_statement($2),
    _pgr_get_statement($3),
    $4) AS a;
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;


-- COMMENTS

COMMENT ON FUNCTION pgr_trsp(TEXT, TEXT, BIGINT, BIGINT, BOOLEAN)
IS 'pgr_trsp(one to one)
- PROPOSED
- Parameters
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - Restrictions SQL with columns: cost, path
  - Departure vertex identifier
  - Destination vertex identifier
- Optional parameters
  - directed
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_trsp.html
';

COMMENT ON FUNCTION pgr_trsp(TEXT, TEXT, BIGINT, ANYARRAY, BOOLEAN)
IS 'pgr_trsp(one to many)
- PROPOSED
- Parameters
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - Restrictions SQL with columns: cost, path
  - Departure vertex identifier
  - Destinations ARRAY[vertices identifier]
- Optional parameters
  - directed := true
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_trsp.html
';

COMMENT ON FUNCTION pgr_trsp(TEXT, TEXT, ANYARRAY, BIGINT, BOOLEAN)
IS 'pgr_trsp(many to one)
- PROPOSED
- Parameters
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - Restrictions SQL with columns: cost, path
  - Departures ARRAY[vertices identifier]
  - Destination vertex identifier
- Optional parameters
  - directed := true
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_trsp.html
';

COMMENT ON FUNCTION pgr_trsp(TEXT, TEXT, ANYARRAY, ANYARRAY, BOOLEAN)
IS 'pgr_trsp(many to many)
- PROPOSED
- Parameters
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - Restrictions SQL with columns: cost, path
  - Departures ARRAY[vertices identifier]
  - Destinations ARRAY[vertices identifier]
- Optional parameters
  - directed := true
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_trsp.html
';

COMMENT ON FUNCTION pgr_trsp(TEXT, TEXT, TEXT, BOOLEAN)
IS 'pgr_trsp(combinations)
- PROPOSED
- Parameters
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - Restrictions SQL with columns: cost, path
  - Combinations SQL with columns: source, target
- Optional parameters
  - directed := true
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_trsp.html
';


-- ONE to ONE
--v2.6
CREATE FUNCTION _pgr_trsp(
    TEXT, -- edges_sql
    TEXT, -- restrictions_sql
    BIGINT, -- start_vid
    BIGINT, -- end_vid
    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.node, a.edge, a.cost, a.agg_cost
        FROM _trsp(
            _pgr_get_statement($1),
            _pgr_get_statement($2),
            ARRAY[$3]::BIGINT[],
            ARRAY[$4]::BIGINT[],
            directed) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- ONE to MANY
--v2.6
CREATE FUNCTION _pgr_trsp(
    TEXT, -- edges_sql
    TEXT, -- restrictions_sql
    BIGINT, -- start_vid
    ANYARRAY, -- end_vids
    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.end_vid, a.node, a.edge, a.cost, a.agg_cost
        FROM _trsp(
            _pgr_get_statement($1),
            _pgr_get_statement($2),
            ARRAY[$3]::BIGINT[],
            $4::bigint[],
            directed) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- MANY to ONE
--v2.6
CREATE FUNCTION _pgr_trsp(
    TEXT, -- edges_sql
    TEXT, -- restrictions_sql
    ANYARRAY, -- start_vids
    BIGINT, -- end_vid
    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.start_vid, a.node, a.edge, a.cost, a.agg_cost
        FROM _trsp(
            _pgr_get_statement($1),
            _pgr_get_statement($2),
            $3::bigint[],
            ARRAY[$4]::BIGINT[],
            $5) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- MANY to MANY
--v2.6
CREATE FUNCTION _pgr_trsp(
    TEXT, -- edges_sql
    TEXT, -- restrictions_sql
    ANYARRAY, -- start_vids
    ANYARRAY, -- end_vids
    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.start_vid, a.end_vid, a.node, a.edge, a.cost, a.agg_cost
        FROM _trsp(
            _pgr_get_statement($1),
            _pgr_get_statement($2),
            $3::bigint[],
            $4::bigint[],
            $5) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- COMMENTS

COMMENT ON FUNCTION _pgr_trsp(TEXT, TEXT, BIGINT, BIGINT, BOOLEAN) IS 'pgRouting internal function';
COMMENT ON FUNCTION _pgr_trsp(TEXT, TEXT, BIGINT, ANYARRAY, BOOLEAN) IS 'pgRouting internal function';
COMMENT ON FUNCTION _pgr_trsp(TEXT, TEXT, ANYARRAY, BIGINT, BOOLEAN) IS 'pgRouting internal function';
COMMENT ON FUNCTION _pgr_trsp(TEXT, TEXT, ANYARRAY, ANYARRAY, BOOLEAN) IS 'pgRouting internal function';


--------------
-- _pgr_trsp
--------------


--v2.6
CREATE FUNCTION _pgr_trsp(
    sql text,
    source_eid integer,
    source_pos float8,
    target_eid integer,
    target_pos float8,
    directed boolean,
    has_reverse_cost boolean,
    turn_restrict_sql text DEFAULT null,

    OUT seq INTEGER,
    OUT id1 INTEGER,
    OUT id2 INTEGER,
    OUT cost FLOAT
)
RETURNS SETOF record
AS 'MODULE_PATHNAME'
LANGUAGE 'c' IMMUTABLE;

-- COMMENTS


COMMENT ON FUNCTION _pgr_trsp(TEXT, INTEGER, FLOAT, INTEGER, FLOAT, BOOLEAN, BOOLEAN, TEXT)
IS 'pgRouting internal function';


--v2.6
CREATE FUNCTION _pgr_array_reverse(anyarray) RETURNS anyarray AS $$
SELECT ARRAY(
    SELECT $1[i]
    FROM generate_subscripts($1,1) AS s(i)
    ORDER BY i DESC
);
$$ LANGUAGE 'sql' STRICT IMMUTABLE;


-- COMMENTS

COMMENT ON FUNCTION _pgr_array_reverse(ANYARRAY)
IS 'pgRouting internal function';


--v3.0
CREATE FUNCTION pgr_trsp(
    TEXT, -- edges SQL (required)
    INTEGER, -- from_vid (required)
    INTEGER, -- to_vid (required)
    BOOLEAN, -- directed (required)
    BOOLEAN, -- has_rcost (required)

    restrictions_sql TEXT DEFAULT NULL,

    OUT seq INTEGER,
    OUT id1 INTEGER,
    OUT id2 INTEGER,
    OUT cost FLOAT
)
RETURNS SETOF record AS
$BODY$
DECLARE
    edges_sql TEXT    := $1;
    start_vid INTEGER := $2;
    end_vid INTEGER   := $3;
    directed BOOLEAN  := $4;
    has_rcost BOOLEAN := $5;

has_reverse BOOLEAN;
new_sql TEXT;
restrictions_query TEXT;
trsp_sql TEXT;
BEGIN
  RAISE WARNING 'pgr_trsp(text,integer,integer,boolean,boolean) deprecated on v3.4.0';
    has_reverse =_pgr_parameter_check('dijkstra', edges_sql, false);

    new_sql := edges_sql;
    IF (has_reverse != has_rcost) THEN  -- user contradiction
        IF (has_reverse) THEN  -- it has reverse_cost but user don't want it.
            -- to be on the safe side because it reads the data wrong, sending only postitive values
            new_sql :=
            'WITH old_sql AS (' || edges_sql || ')' ||
            '   SELECT id, source, target, cost FROM old_sql';
        ELSE -- it does not have reverse_cost but user wants it
            RAISE EXCEPTION 'Error, reverse_cost is used, but query did''t return ''reverse_cost'' column'
            USING ERRCODE := 'XX000';
        END IF;
    END IF;

    IF (restrictions_sql IS NULL OR length(restrictions_sql) = 0) THEN
        -- no restrictions then its a dijkstra
        RETURN query SELECT a.seq - 1 AS seq, node::INTEGER AS id1, edge::INTEGER AS id2, a.cost
        FROM pgr_dijkstra(new_sql, start_vid, end_vid, directed) a;
        RETURN;
    END IF;


    restrictions_query = $$
        WITH old_restrictions AS ( $$ ||
            $6 || $$
        )
        SELECT ROW_NUMBER() OVER() AS id,
            _pgr_array_reverse(array_prepend(target_id, string_to_array(via_path::text, ',')::INTEGER[])) AS path,
            to_cost AS cost
        FROM old_restrictions;
    $$;



    RETURN query
        SELECT (a.seq - 1)::INTEGER, a.node::INTEGER, a.edge::INTEGER, a.cost
        FROM _pgr_trsp(new_sql, restrictions_query, start_vid, end_vid, directed) AS a;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'Error computing path: Path Not Found';
    END IF;

END
$BODY$
LANGUAGE plpgsql VOLATILE
COST 100
ROWS 1000;



--v3.0
CREATE FUNCTION pgr_trsp(
    TEXT,    -- sql (required)
    INTEGER, -- source_eid (required)
    FLOAT,   -- source_pos (required)
    INTEGER, -- target_eid (required)
    FLOAT,   -- target_pos (required)
    BOOLEAN, -- directed (required)
    BOOLEAN, -- has_reverse_cost (required)

    turn_restrict_sql text DEFAULT null,

    OUT seq INTEGER,
    OUT id1 INTEGER,
    OUT id2 INTEGER,
    OUT cost FLOAT
)
RETURNS SETOF record AS
$BODY$
DECLARE
    sql TEXT                 := $1;
    source_eid INTEGER       := $2;
    source_pos FLOAT         := $3;
    target_eid INTEGER       := $4;
    target_pos FLOAT         := $5;
    directed BOOLEAN         := $6;
    has_reverse_cost BOOLEAN := $7;

has_reverse BOOLEAN;
new_sql TEXT;
trsp_sql TEXT;
source_sql TEXT;
target_sql TEXT;
union_sql TEXT;
union_sql1 TEXT;
union_sql2 TEXT;
final_sql TEXT;

BEGIN
    IF $2 IS NULL OR $3 IS NULL OR $4 IS NULL OR $5 IS NULL OR $6 IS NULL THEN
        RETURN;
    END IF;
  RAISE WARNING 'pgr_trsp(text,integer,float,integer,float,boolean,boolean) deprecated on v3.4.0';
    has_reverse =_pgr_parameter_check('dijkstra', sql, false);

    new_sql := sql;
    IF (has_reverse != has_reverse_cost) THEN  -- user contradiction
        IF (has_reverse) THEN
            -- it has reverse_cost but user don't want it.
            -- to be on the safe side because it reads the data wrong, sending only postitive values
            new_sql :=
            'WITH old_sql AS (' || sql || ')' ||
            '   SELECT id, source, target, cost FROM old_sql';
        ELSE -- it does not have reverse_cost but user wants it
            RAISE EXCEPTION 'Error, reverse_cost is used, but query did''t return ''reverse_cost'' column'
            USING ERRCODE := 'XX000';
        END IF;
    END IF;

    IF (turn_restrict_sql IS NULL OR length(turn_restrict_sql) = 0) THEN
        -- no restrictions then its a withPoints or dijkstra
        IF source_pos = 0 THEN
            source_sql = '(SELECT source FROM (' || sql || ') b WHERE id = ' ||  source_eid || ')';
        ELSE IF source_pos = 1 THEN
            source_sql = '(SELECT target FROM (' || sql || ') b WHERE id = ' || source_eid || ')';
        ELSE
            source_sql = '-1';
            union_sql1 =  '(SELECT 1 as pid, ' || source_eid || ' as edge_id, ' || source_pos || '::float8 as fraction)';
        END IF;
        END IF;
        -- raise notice 'source_sql %', source_sql;
        -- raise notice 'union_sql1 %', union_sql1;


        IF target_pos = 0 THEN
            target_sql = '(SELECT source FROM (' || sql || ') c WHERE id = ' ||  target_eid || ')';
        ELSE IF target_pos = 1 THEN
            target_sql = '(SELECT target FROM (' || sql || ') c WHERE id = ' ||  target_eid || ')';
        ELSE
            target_sql = '-2';
            union_sql2 =  ' (SELECT 2 as pid, ' || target_eid || ' as edge_id, ' || target_pos || '::float8 as fraction)';
        END IF;
        END IF;

        -- raise notice 'target_sql %', target_sql;
        -- raise notice 'union_sql2 %', union_sql2;

        IF union_sql1 IS NOT NULL AND union_sql2 IS NOT NULL THEN
            union_sql = union_sql1 || ' UNION ' || union_sql2;
        ELSE IF union_sql1 IS NOT NULL AND union_sql2 IS NULL THEN
            union_sql = union_sql1;
        ELSE IF union_sql1 IS NULL AND union_sql2 IS NOT NULL THEN
            union_sql = union_sql2;
        END IF;
        END IF;
        END IF;

        IF union_sql IS NULL THEN
            -- no points then its a dijkstra
            final_sql = 'WITH final_sql AS (
                 SELECT  a.seq-1 AS seq, node::INTEGER AS id1, edge::INTEGER AS id2, cost FROM pgr_dijkstra($$' || new_sql || '$$
                ,' || source_sql || '
                ,' || target_sql || '
                , directed := ' || directed || '
            ) a )
            SELECT seq, id1, id2, cost  FROM final_sql ORDER BY seq';
        ELSE
            -- points then its a withPoints
            final_sql = 'WITH final_sql AS (
                SELECT  a.seq-1 AS seq, node::INTEGER AS id1, edge::INTEGER AS id2, cost FROM pgr_withpoints($$' || new_sql || '$$
                , $$' || union_sql || '$$
                ,' || source_sql || '
                ,' || target_sql || '
                , directed := ' || directed || '
            ) a )
            SELECT seq, CASE WHEN seq = 0 AND ' || source_pos || '=0 THEN id1
                             WHEN seq = 0 AND ' || source_pos || '!=0 THEN -1
                             WHEN id2 = -1 AND ' || target_pos || '=0 THEN id1
                             WHEN id2 = -1 AND ' || target_pos || '!=0 THEN id1
                             ELSE id1 END AS id1, id2, cost  FROM final_sql ORDER BY seq';
        END IF;


        -- raise notice 'final_sql %', final_sql;
        RETURN QUERY EXECUTE final_sql;
        RETURN;

    END IF;

    -- with restrictions calls the original code
    RETURN query SELECT * FROM _pgr_trsp(new_sql, source_eid, source_pos, target_eid, target_pos, directed, has_reverse_cost, turn_restrict_sql);
    RETURN;

END
$BODY$
LANGUAGE plpgsql VOLATILE
COST 100
ROWS 1000;

-- COMMENTS

COMMENT ON FUNCTION pgr_trsp(TEXT, INTEGER, INTEGER, BOOLEAN, BOOLEAN, TEXT)
IS 'pgr_trsp
- DEPRECATED signature on v3.4.0
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_trsp.html
';


COMMENT ON FUNCTION pgr_trsp(TEXT, INTEGER, FLOAT, INTEGER, FLOAT, BOOLEAN, BOOLEAN, TEXT)
IS 'pgr_trsp
- DEPRECATED signature on v3.4.0
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_trsp_withPoints.html
';


-----------------------
-- _pgr_trspViaVertices
-----------------------


--v3.0
CREATE FUNCTION _pgr_trspViaVertices
    (sql text,
    vids integer[],
    directed boolean,
    has_rcost boolean,
    turn_restrict_sql text DEFAULT NULL,

    OUT seq INTEGER,
    OUT id1 INTEGER,
    OUT id2 INTEGER,
    OUT id3 INTEGER,
    OUT cost FLOAT
)
RETURNS SETOF RECORD AS
$body$

declare
    i integer;
    rr RECORD;
    lrr RECORD;
    lrra boolean := false;
    seq1 integer := 0;
    seq2 integer := 0;
    restrictions_query TEXT;

begin
    IF (turn_restrict_sql IS NULL) THEN
        RAISE EXCEPTION 'Restrictions Missing';
    END IF;

    restrictions_query = $$
    WITH old_restrictions AS ( $$ ||
        $5 || $$
    )
    SELECT ROW_NUMBER() OVER() AS id,
    _pgr_array_reverse(array_prepend(target_id, string_to_array(via_path, ',')::INTEGER[])) AS path,
    to_cost AS cost
    FROM old_restrictions;
    $$;


    -- loop through each pair of vids and compute the path
    for i in 1 .. array_length(vids, 1)-1 loop
        seq2 := seq2 + 1;
        for rr in select a.seq, seq2 as id1, a.node::INTEGER as id2, a.edge::INTEGER as id3, a.cost
                    from _pgr_trsp(sql, restrictions_query, vids[i], vids[i+1], directed) as a loop
            -- filter out the individual path ends except the last one
            -- we might not want to do this so we can know where the via points are in the path result
            -- but this needs more thought
            --raise notice 'rr: %', rr;
            if rr.id3 = -1 then
                lrr := rr;
                lrra := true;
            else
                seq1 := seq1 + 1;
                rr.seq := seq1;

                seq := rr.seq;
                id1 := rr.id1;
                id2 := rr.id2;
                id3 := rr.id3;
                cost := rr.cost;
                return next;
            end if;
        end loop;
    end loop;

    if lrra then
        seq1 := seq1 + 1;
        lrr.seq := seq1;

        seq := lrr.seq;
        id1 := lrr.id1;
        id2 := lrr.id2;
        id3 := lrr.id3;
        cost := lrr.cost;
        return next;
    end if;
    return;
end;
$body$
language plpgsql stable
cost 100
rows 1000;

-- COMMENTS

COMMENT ON FUNCTION _pgr_trspViaVertices(TEXT, INTEGER [], BOOLEAN, BOOLEAN, TEXT)
IS 'pgRouting internal function';



--v3.0
CREATE FUNCTION pgr_trspViaVertices(
    TEXT, -- edges SQL (required)
    ANYARRAY,  -- via vids (required)
    BOOLEAN, -- directed (required)
    BOOLEAN, -- has_rcost (required)

    restrictions_sql TEXT DEFAULT NULL,

    OUT seq INTEGER,
    OUT id1 INTEGER,
    OUT id2 INTEGER,
    OUT id3 INTEGER,
    OUT cost FLOAT
)
RETURNS SETOF RECORD AS

$BODY$
DECLARE
    edges_sql TEXT     := $1;
    via_vids INTEGER[] := $2;
    directed BOOLEAN   := $3;
    has_rcost BOOLEAN  := $4;

has_reverse BOOLEAN;
new_sql TEXT;
BEGIN
  RAISE WARNING 'pgr_trspViaVertices(text,anyarray,boolean,boolean,text) is been deprecated';

    has_reverse =_pgr_parameter_check('dijkstra', edges_sql, false);

    new_sql := edges_sql;
    IF (has_reverse != has_rcost) THEN  -- user contradiction
        IF (has_reverse) THEN  -- it has reverse_cost but user don't want it.
            new_sql :=
               'WITH old_sql AS (' || edges_sql || ')' ||
                '   SELECT id, source, target, cost FROM old_sql';
        ELSE -- it does not have reverse_cost but user wants it
            RAISE EXCEPTION 'Error, reverse_cost is used, but query did''t return ''reverse_cost'' column'
            USING ERRCODE := 'XX000';
        END IF;
    END IF;

    IF (restrictions_sql IS NULL OR length(restrictions_sql) = 0) THEN
        RETURN query SELECT (row_number() over())::INTEGER, path_id:: INTEGER, node::INTEGER,
            (CASE WHEN edge = -2 THEN -1 ELSE edge END)::INTEGER, a.cost
            FROM pgr_dijkstraVia(new_sql, via_vids, directed, strict:=true) AS a WHERE edge != -1;
        RETURN;
    END IF;


    -- make the call without contradiction from part of the user
    RETURN query SELECT * FROM _pgr_trspViaVertices(new_sql, via_vids::INTEGER[], directed, has_rcost, restrictions_sql);
    IF NOT FOUND THEN
        RAISE EXCEPTION 'Error computing path: Path Not Found';
    END IF;
END
$BODY$
LANGUAGE plpgsql VOLATILE
COST 100
ROWS 1000;

-- COMMENTS

COMMENT ON FUNCTION pgr_trspViaVertices(TEXT, ANYARRAY, BOOLEAN, BOOLEAN, TEXT)
IS 'pgr_trspViaVertices
- DEPRECATED function on v3.4.0
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_trspVia.html
';



--v3.0
CREATE FUNCTION pgr_trspViaEdges(
    text,      -- SQL (required)
    integer[], -- eids (required)
    FLOAT[],   -- pcts (required)
    BOOLEAN,   -- directed (required)
    BOOLEAN,   -- has_rcost (requierd)

    turn_restrict_sql text DEFAULT NULL::text,

    OUT seq INTEGER,
    OUT id1 INTEGER,
    OUT id2 INTEGER,
    OUT id3 INTEGER,
    OUT cost FLOAT
)
RETURNS SETOF RECORD AS
$body$

declare
    sql TEXT          := $1;
    eids INTEGER[]    := $2;
    pcts FLOAT[]      := $3;
    directed BOOLEAN  := $4;
    has_rcost BOOLEAN := $5;

    i integer;
    rr RECORD;
    lrr RECORD;
    first boolean := true;
    seq1 integer := 0;
    seq2 integer :=0;
    has_reverse BOOLEAN;
    point_is_vertex BOOLEAN := false;
    edges_sql TEXT;
    f float;

begin
  RAISE WARNING 'pgr_trspViaEdges(text,integer[],float[],boolean,boolean,text) deprecated on v3.4.0';
    SELECT 0::INTEGER AS seq, NULL::INTEGER AS id1, NULL::INTEGER AS id2, NULL::INTEGER AS id3, NULL::FLOAT AS cost INTO lrr;
    has_reverse =_pgr_parameter_check('dijkstra', sql, false);
    edges_sql := sql;
    IF (has_reverse != has_rcost) THEN
        IF (NOT has_rcost) THEN
            -- user does not want to use reverse cost column
            edges_sql = 'SELECT id, source, target, cost FROM (' || sql || ') a';
        ELSE
            raise EXCEPTION 'has_rcost set to true but reverse_cost not found';
        END IF;
    END IF;

    FOREACH f IN ARRAY pcts LOOP
        IF f in (0,1) THEN
           point_is_vertex := true;
        END IF;
    END LOOP;

    IF (turn_restrict_sql IS NULL OR length(turn_restrict_sql) = 0) AND NOT point_is_vertex THEN
        -- no restrictions then its a _pgr_withPointsVia
        RETURN query SELECT a.seq::INTEGER, path_id::INTEGER AS id1, node::INTEGER AS id2, edge::INTEGER AS id3, a.cost
        FROM _pgr_withPointsVia(edges_sql, eids, pcts, directed) a;
        RETURN;
    END IF;

    if array_length(eids, 1) != array_length(pcts, 1) then
        raise exception 'The length of arrays eids and pcts must be the same!';
    end if;

    -- loop through each pair of vids and compute the path
    for i in 1 .. array_length(eids, 1)-1 loop
        seq2 := seq2 + 1;
        for rr in select a.seq, seq2 as id1, a.id1 as id2, a.id2 as id3, a.cost
                    from pgr_trsp(edges_sql,
                                  eids[i], pcts[i],
                                  eids[i+1], pcts[i+1],
                                  directed,
                                  has_rcost,
                                  turn_restrict_sql) as a loop
            -- combine intermediate via costs when cost is split across
            -- two parts of a segment because it stops it and
            -- restarts the next leg also on it
            -- we might not want to do this so we can know where the via points are in the path result
            -- but this needs more thought
            --
            -- there are multiple condition we have to deal with
            -- between the end of one leg and start of the next
            -- 1. same vertex_id. edge_id=-1; drop record with edge_id=-1
            -- means: path ends on vertex
            -- NOTICE:  rr: (19,1,44570022,-1,0)
            -- NOTICE:  rr: (0,2,44570022,1768045,2.89691196717448)
            -- 2. vertex_id=-1; sum cost components
            -- means: path end/starts with the segment
            -- NOTICE:  rr: (11,2,44569628,1775909,9.32885885148532)
            -- NOTICE:  rr: (0,3,-1,1775909,0.771386350984395)

            --raise notice 'rr: %', rr;
            if first then
                lrr := rr;
                first := false;
            else
                if lrr.id3 = -1 then
                    lrr := rr;
                elsif lrr.id3 = rr.id3 then
                    lrr.cost := lrr.cost + rr.cost;
                    if rr.id2 = -1 then
                        rr.id2 := lrr.id2;
                    end if;
                else
                    seq1 := seq1 + 1;
                    lrr.seq := seq1;

                    seq := lrr.seq;
                    id1 := lrr.id1;
                    id2 := lrr.id2;
                    id3 := lrr.id3;
                    cost := lrr.cost;
                    return next;
                    lrr := rr;
                end if;
            end if;
        end loop;
    end loop;

    seq1 := seq1 + 1;
    lrr.seq := seq1;

    seq := lrr.seq;
    id1 := lrr.id1;
    id2 := lrr.id2;
    id3 := lrr.id3;
    cost := lrr.cost;
    return next;
    return;
end;
$body$
language plpgsql stable
cost 100
rows 1000;


-- COMMENTS

COMMENT ON FUNCTION pgr_trspViaEdges(TEXT, INTEGER[], FLOAT[], BOOLEAN, BOOLEAN, TEXT)
IS 'pgr_trspViaEdges
- DEPRECATED function on v3.4.0
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_trspVia_withPoints.html
';



--v3.4
CREATE FUNCTION _pgr_trsp_withPoints(
    TEXT,  -- edges_sql
    TEXT,  -- restrictions_sql
    TEXT,  -- points_sql
    ANYARRAY,  -- start_vid
    ANYARRAY,  -- end_vids

    directed BOOLEAN,
    driving_side CHAR,
    details BOOLEAN,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT departure BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c VOLATILE;


--v3.4
CREATE FUNCTION _pgr_trsp_withPoints(
    TEXT,  -- edges_sql
    TEXT,  -- restrictions_sql
    TEXT,  -- points_sql
    TEXT,  -- combinations_sql

    directed BOOLEAN,
    driving_side CHAR,
    details BOOLEAN,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT departure BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE C VOLATILE STRICT;

-- COMMENTS
COMMENT ON FUNCTION _pgr_trsp_withPoints(TEXT, TEXT, TEXT, ANYARRAY, ANYARRAY, BOOLEAN, CHAR, BOOLEAN)
IS 'pgRouting internal function';

COMMENT ON FUNCTION _pgr_trsp_withPoints(TEXT, TEXT, TEXT, TEXT, BOOLEAN, CHAR, BOOLEAN)
IS 'pgRouting internal function';



-- ONE TO ONE
--v3.4
CREATE FUNCTION pgr_trsp_withPoints(
  TEXT, -- Edges SQL
  TEXT, -- Restrictions SQL
  TEXT, -- Points SQL
  BIGINT, -- start_vid
  BIGINT, -- end_vid

  directed BOOLEAN DEFAULT true,
  driving_side CHAR DEFAULT 'r', -- 'r'/'l'
  details BOOLEAN DEFAULT false,

  OUT seq INTEGER,
  OUT path_seq INTEGER,
  OUT start_vid BIGINT,
  OUT end_vid BIGINT,
  OUT node BIGINT,
  OUT edge BIGINT,
  OUT cost FLOAT,
  OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
SELECT *
FROM _pgr_trsp_withPoints(
  _pgr_get_statement($1),
  _pgr_get_statement($2),
  _pgr_get_statement($3),
  ARRAY[$4]::BIGINT[], ARRAY[$5]::BIGINT[], $6, $7, $8);
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;


-- ONE TO MANY
--v3.4
CREATE FUNCTION pgr_trsp_withPoints(
  TEXT, -- Edges SQL
  TEXT, -- Restrictions SQL
  TEXT, -- Points SQL
  BIGINT, -- start_vid
  ANYARRAY, -- end_vids

  directed BOOLEAN DEFAULT true,
  driving_side CHAR DEFAULT 'r', -- 'r'/'l'
  details BOOLEAN DEFAULT false,

  OUT seq INTEGER,
  OUT path_seq INTEGER,
  OUT start_vid BIGINT,
  OUT end_vid BIGINT,
  OUT node BIGINT,
  OUT edge BIGINT,
  OUT cost FLOAT,
  OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
SELECT *
FROM _pgr_trsp_withPoints(
  _pgr_get_statement($1),
  _pgr_get_statement($2),
  _pgr_get_statement($3),
  ARRAY[$4]::BIGINT[], $5::BIGINT[], $6, $7, $8);
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;

-- MANY TO ONE
--v3.4
CREATE FUNCTION pgr_trsp_withPoints(
  TEXT, -- Edges SQL
  TEXT, -- Restrictions SQL
  TEXT, -- Points SQL
  ANYARRAY, -- start_vids
  BIGINT, -- end_vid

  directed BOOLEAN DEFAULT true,
  driving_side CHAR DEFAULT 'r', -- 'r'/'l'
  details BOOLEAN DEFAULT false,

  OUT seq INTEGER,
  OUT path_seq INTEGER,
  OUT start_vid BIGINT,
  OUT end_vid BIGINT,
  OUT node BIGINT,
  OUT edge BIGINT,
  OUT cost FLOAT,
  OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
SELECT *
FROM _pgr_trsp_withPoints(
  _pgr_get_statement($1),
  _pgr_get_statement($2),
  _pgr_get_statement($3),
  $4::BIGINT[], ARRAY[$5]::BIGINT[], $6, $7, $8);
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;

-- MANY TO MANY
--v3.4
CREATE FUNCTION pgr_trsp_withPoints(
  TEXT, -- Edges SQL
  TEXT, -- Restrictions SQL
  TEXT, -- Points SQL
  ANYARRAY, -- start_vids
  ANYARRAY, -- end_vids

  directed BOOLEAN DEFAULT true,
  driving_side CHAR DEFAULT 'r', -- 'r'/'l'
  details BOOLEAN DEFAULT false,

  OUT seq INTEGER,
  OUT path_seq INTEGER,
  OUT start_vid BIGINT,
  OUT end_vid BIGINT,
  OUT node BIGINT,
  OUT edge BIGINT,
  OUT cost FLOAT,
  OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
SELECT *
FROM _pgr_trsp_withPoints(
  _pgr_get_statement($1),
  _pgr_get_statement($2),
  _pgr_get_statement($3),
  $4::BIGINT[], $5::BIGINT[], $6, $7, $8);
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;


-- Combinations
--v3.4
CREATE FUNCTION pgr_trsp_withPoints(
  TEXT, -- Edges SQL
  TEXT, -- Restrictions SQL
  TEXT, -- Points SQL
  TEXT, -- combinations SQL

  directed BOOLEAN DEFAULT true,
  driving_side CHAR DEFAULT 'r', -- 'r'/'l'
  details BOOLEAN DEFAULT false,

  OUT seq INTEGER,
  OUT path_seq INTEGER,
  OUT start_vid BIGINT,
  OUT end_vid BIGINT,
  OUT node BIGINT,
  OUT edge BIGINT,
  OUT cost FLOAT,
  OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
SELECT *
FROM _pgr_trsp_withPoints(
  _pgr_get_statement($1),
  _pgr_get_statement($2),
  _pgr_get_statement($3),
  _pgr_get_statement($4),
  $5, $6, $7);
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;

-- COMMENTS
COMMENT ON FUNCTION pgr_trsp_withPoints(TEXT, TEXT, TEXT, BIGINT, BIGINT, BOOLEAN, CHAR, BOOLEAN)
IS 'pgr_trsp_withPoints (One to One)
- PROPOSED
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - Restrictions SQL with columns: id, cost, path
  - Points SQL with columns: [pid], edge_id, fraction [,side]
  - Departure vertex/point identifier
  - Destination vertex/point identifier
- Optional Parameters:
  - directed := ''true''
  - driving_side := ''r''
  - details := ''false''
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_trsp_withPoints.html
';


COMMENT ON FUNCTION pgr_trsp_withPoints(TEXT, TEXT, TEXT, BIGINT, ANYARRAY, BOOLEAN, CHAR, BOOLEAN)
IS 'pgr_trsp_withPoints(One to Many)
- PROPOSED
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - Restrictions SQL with columns: id, cost, path
  - Points SQL with columns: [pid], edge_id, fraction [,side]
  - Departure vertex/point identifier
  - Destinations ARRAY[vertices/Points identifier]
- Optional Parameters:
  - directed := ''true''
  - driving_side := ''r''
  - details := ''false''
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_trsp_withPoints.html
';

COMMENT ON FUNCTION pgr_trsp_withPoints(TEXT, TEXT, TEXT, ANYARRAY, BIGINT, BOOLEAN, CHAR, BOOLEAN)
IS 'pgr_trsp_withPoints(Many to One)
- PROPOSED
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - Restrictions SQL with columns: id, cost, path
  - Points SQL with columns: [pid], edge_id, fraction [,side]
  - Departures ARRAY[vertices/Points identifier]
  - Destination vertex/point identifier
- Optional Parameters:
  - directed := ''true''
  - driving_side := ''r''
  - details := ''false''
- Documentation:
- https://docs.pgrouting.org/latest/en/pgr_trsp_withPoints.html
';

COMMENT ON FUNCTION pgr_trsp_withPoints(TEXT, TEXT, TEXT, ANYARRAY, ANYARRAY, BOOLEAN, CHAR, BOOLEAN)
IS 'pgr_trsp_withPoints(Many to Many)
- PROPOSED
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - Restrictions SQL with columns: id, cost, path
  - Points SQL with columns: [pid], edge_id, fraction [,side]
  - Departures ARRAY[vertices/Points identifier]
  - Destinations ARRAY[vertices/Points identifier]
- Optional Parameters:
  - directed := ''true''
  - driving_side := ''r''
  - details := ''false''
- Documentation:
- https://docs.pgrouting.org/latest/en/pgr_trsp_withPoints.html
';

COMMENT ON FUNCTION pgr_trsp_withPoints(TEXT, TEXT, TEXT, TEXT, BOOLEAN, CHAR, BOOLEAN)
IS 'pgr_trsp_withPoints(Combinations)
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - Restrictions SQL with columns: id, path, cost
  - Points SQL with columns: [pid], edge_id, fraction [,side]
  - Combinations SQL with columns: source, target
- Optional Parameters:
  - directed := ''true''
  - driving_side := ''r''
  - details := ''false''
- Documentation:
- https://docs.pgrouting.org/latest/en/pgr_trsp_withPoints.html
';


--v3.4
CREATE FUNCTION pgr_trspVia(
  TEXT,     -- edges sql
  TEXT,     -- restrictions sql
  ANYARRAY, -- via vids

  directed BOOLEAN DEFAULT true,
  strict BOOLEAN DEFAULT false,
  U_turn_on_edge BOOLEAN DEFAULT true,

  OUT seq INTEGER,
  OUT path_id INTEGER,
  OUT path_seq INTEGER,
  OUT start_vid BIGINT,
  OUT end_vid BIGINT,
  OUT node BIGINT,
  OUT edge BIGINT,
  OUT cost FLOAT,
  OUT agg_cost FLOAT,
  OUT route_agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
SELECT *
FROM _pgr_trspVia(
  _pgr_get_statement($1),
  _pgr_get_statement($2),
  $3 , $4, $5, $6);
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;

-- COMMENTS

COMMENT ON FUNCTION pgr_trspVia(TEXT, TEXT, ANYARRAY, BOOLEAN, BOOLEAN, BOOLEAN)
IS 'pgr_trspVia
- PROPOSED
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - Restrictions SQL with columns: cost, path
  - ARRAY[via vertices identifiers]
- Optional Parameters
  - directed := true
  - strict := false
  - U_turn_on_edge := true
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_trspVia.html
';


------------------
-- pgr_trspVia
------------------

--v3.0
CREATE FUNCTION _pgr_trspVia(
  TEXT, -- edges
  TEXT, -- restrictions
  ANYARRAY, -- via
  BOOLEAN,  -- directed
  BOOLEAN,  -- strict
  BOOLEAN,  -- U_turn_on_edge

  OUT seq INTEGER,
  OUT path_id INTEGER,
  OUT path_seq INTEGER,
  OUT start_vid BIGINT,
  OUT end_vid BIGINT,
  OUT node BIGINT,
  OUT edge BIGINT,
  OUT cost FLOAT,
  OUT agg_cost FLOAT,
  OUT route_agg_cost FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_trspVia(TEXT, TEXT, ANYARRAY, BOOLEAN, BOOLEAN, BOOLEAN)
IS 'pgRouting internal function';


--v3.4
CREATE FUNCTION pgr_trspVia_withPoints(
  TEXT,     -- edges sql
  TEXT,     -- restrictions sql
  TEXT,     -- points
  ANYARRAY, -- via vids

  directed BOOLEAN DEFAULT true,

  -- via parameters
  strict BOOLEAN DEFAULT false,
  U_turn_on_edge BOOLEAN DEFAULT true,

  -- withPoints parameters
  driving_side CHAR DEFAULT 'r', -- 'r'/'l'
  details BOOLEAN DEFAULT false,

  OUT seq INTEGER,
  OUT path_id INTEGER,
  OUT path_seq INTEGER,
  OUT start_vid BIGINT,
  OUT end_vid BIGINT,
  OUT node BIGINT,
  OUT edge BIGINT,
  OUT cost FLOAT,
  OUT agg_cost FLOAT,
  OUT route_agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
SELECT *
FROM _pgr_trspVia_withPoints(
  _pgr_get_statement($1),
  _pgr_get_statement($2),
  _pgr_get_statement($3),
  $4, $5, $6, $7, $8, $9);
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;

-- COMMENTS

COMMENT ON FUNCTION pgr_trspVia_withPoints(TEXT, TEXT, TEXT, ANYARRAY, BOOLEAN, BOOLEAN, BOOLEAN, CHAR, BOOLEAN)
IS 'pgr_trspVia_withPoints
- PROPOSED
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - Restrictions SQL with columns: cost, path
  - Points SQL with columns: [pid], edge_id, fraction [,side]
  - ARRAY[via vertices identifiers]
- Optional Parameters
  - directed := true
  - strict := false
  - U_turn_on_edge := true
  - driving_side := ''r''
  - details := ''false''
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_trspVia_withPoints.html
';


--v3.0
CREATE FUNCTION _pgr_trspVia_withPoints(
  TEXT,     -- edges
  TEXT,     -- restrictions
  TEXT,     -- points
  ANYARRAY, -- via
  -- via parameters
  BOOLEAN,  -- directed
  BOOLEAN,  -- strict
  BOOLEAN,  -- U_turn_on_edge
  -- withPoints parameters
  CHAR,  -- driving_side
  BOOLEAN,  -- details

  OUT seq INTEGER,
  OUT path_id INTEGER,
  OUT path_seq INTEGER,
  OUT start_vid BIGINT,
  OUT end_vid BIGINT,
  OUT node BIGINT,
  OUT edge BIGINT,
  OUT cost FLOAT,
  OUT agg_cost FLOAT,
  OUT route_agg_cost FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_trspVia_withPoints(TEXT, TEXT, TEXT, ANYARRAY, BOOLEAN, BOOLEAN, BOOLEAN, CHAR, BOOLEAN)
IS 'pgRouting internal function';


--------------
--------------
-- maxflow
--------------
--------------


--v2.6
CREATE FUNCTION _pgr_maxflow(
    edges_sql TEXT,
    sources ANYARRAY,
    targets ANYARRAY,

    algorithm INTEGER DEFAULT 1,
    only_flow BOOLEAN DEFAULT false,

    OUT seq INTEGER,
    OUT edge_id BIGINT,
    OUT source BIGINT,
    OUT target BIGINT,
    OUT flow BIGINT,
    OUT residual_capacity BIGINT
    )
  RETURNS SETOF RECORD AS
 'MODULE_PATHNAME'
    LANGUAGE c VOLATILE STRICT;


--v3.2
CREATE FUNCTION _pgr_maxflow(
    edges_sql TEXT,
    combinations_sql TEXT,

    algorithm INTEGER DEFAULT 1,
    only_flow BOOLEAN DEFAULT false,

    OUT seq INTEGER,
    OUT edge_id BIGINT,
    OUT source BIGINT,
    OUT target BIGINT,
    OUT flow BIGINT,
    OUT residual_capacity BIGINT
    )
  RETURNS SETOF RECORD AS
 'MODULE_PATHNAME'
    LANGUAGE c VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_maxflow(TEXT, ANYARRAY, ANYARRAY, INTEGER, BOOLEAN)
IS 'pgRouting internal function';

COMMENT ON FUNCTION _pgr_maxflow(TEXT, TEXT, INTEGER, BOOLEAN)
IS 'pgRouting internal function';


------------------------
------------------------
-- costFlow
------------------------
------------------------


------------------------
-- _pgr_maxFlowMinCost
------------------------


--v3.0
CREATE FUNCTION _pgr_maxFlowMinCost(
    edges_sql TEXT,
    sources ANYARRAY,
    targets ANYARRAY,

    only_cost BOOLEAN DEFAULT false,

    OUT seq INTEGER,
    OUT edge BIGINT,
    OUT source BIGINT,
    OUT target BIGINT,
    OUT flow BIGINT,
    OUT residual_capacity BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c IMMUTABLE STRICT;

--v3.2
CREATE FUNCTION _pgr_maxFlowMinCost(
    edges_sql TEXT,
    combinations_sql TEXT,
    only_cost BOOLEAN DEFAULT false,

    OUT seq INTEGER,
    OUT edge BIGINT,
    OUT source BIGINT,
    OUT target BIGINT,
    OUT flow BIGINT,
    OUT residual_capacity BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c IMMUTABLE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_maxFlowMinCost(TEXT, ANYARRAY, ANYARRAY, BOOLEAN)
IS 'pgRouting internal function';

COMMENT ON FUNCTION _pgr_maxFlowMinCost(TEXT, TEXT, BOOLEAN)
IS 'pgRouting internal function';


-------------------
-- pgr_edmondsKarp
-------------------

-- ONE to ONE
--v2.6
CREATE FUNCTION pgr_edmondsKarp(
    TEXT, -- edges_sql (required)
    BIGINT, -- from_vid (required)
    BIGINT, -- to_vid (required)

    OUT seq INTEGER,
    OUT edge BIGINT,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT flow BIGINT,
    OUT residual_capacity BIGINT)
  RETURNS SEtoF RECORD AS
  $BODY$
        SELECT *
        FROM _pgr_maxflow(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], ARRAY[$3]::BIGINT[], 3);
  $BODY$
  LANGUAGE sql VOLATILE STRICT;


-- ONE to MANY
--v2.6
CREATE FUNCTION pgr_edmondsKarp(
    TEXT, -- edges_sql (required)
    BIGINT, -- from_vid (required)
    ANYARRAY, -- to_vids (required)

    OUT seq INTEGER,
    OUT edge BIGINT,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT flow BIGINT,
    OUT residual_capacity BIGINT)
  RETURNS SEtoF RECORD AS
  $BODY$
        SELECT *
        FROM _pgr_maxflow(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], $3::BIGINT[], 3);
  $BODY$
  LANGUAGE sql VOLATILE STRICT;


-- MANY to ONE
--v2.6
CREATE FUNCTION pgr_edmondsKarp(
    TEXT, -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    BIGINT, -- to_vid (required)

    OUT seq INTEGER,
    OUT edge BIGINT,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT flow BIGINT,
    OUT residual_capacity BIGINT)
  RETURNS SEtoF RECORD AS
  $BODY$
        SELECT *
        FROM _pgr_maxflow(_pgr_get_statement($1), $2::BIGINT[], ARRAY[$3]::BIGINT[], 3);
  $BODY$
  LANGUAGE sql VOLATILE STRICT;


-- MANY to MANY
--v2.6
CREATE FUNCTION pgr_edmondsKarp(
    TEXT, -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    ANYARRAY, -- to_vids (required)

    OUT seq INTEGER,
    OUT edge BIGINT,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT flow BIGINT,
    OUT residual_capacity BIGINT)
  RETURNS SEtoF RECORD AS
  $BODY$
        SELECT *
        FROM _pgr_maxflow(_pgr_get_statement($1), $2::BIGINT[], $3::BIGINT[], 3);
  $BODY$
  LANGUAGE sql VOLATILE STRICT;

-- COMBINATIONS
--v3.2
CREATE FUNCTION pgr_edmondsKarp(
    TEXT, -- edges_sql (required)
    TEXT, -- combinations_sql (required)

    OUT seq INTEGER,
    OUT edge BIGINT,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT flow BIGINT,
    OUT residual_capacity BIGINT)
  RETURNS SEtoF RECORD AS
  $BODY$
        SELECT *
        FROM _pgr_maxflow(_pgr_get_statement($1), _pgr_get_statement($2), 3);
  $BODY$
  LANGUAGE sql VOLATILE STRICT;


-- COMMENTS


COMMENT ON FUNCTION pgr_edmondsKarp(TEXT, BIGINT, BIGINT)
IS 'pgr_edmondsKarp(One to One)
- Directed graph
- Parameters:
   - Edges SQL with columns: id, source, target, capacity [,reverse_capacity]
   - From vertex
   - to vertex
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_edmondsKarp.html
';


COMMENT ON FUNCTION pgr_edmondsKarp(TEXT, BIGINT, ANYARRAY)
IS 'pgr_edmondsKarp(One to Many)
- Directed graph
- Parameters:
  - Edges SQL with columns: id, source, target, capacity [,reverse_capacity]
  - From vertex
  - to ARRAY[vertices identifiers]
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_edmondsKarp.html
';


COMMENT ON FUNCTION pgr_edmondsKarp(TEXT, ANYARRAY, BIGINT)
IS 'pgr_edmondsKarp(Many to One)
- Directed graph
- Parameters:
  - Edges SQL with columns: id, source, target, capacity [,reverse_capacity]
  - From ARRAY[vertices identifiers]
  - to vertex
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_edmondsKarp.html
';


COMMENT ON FUNCTION pgr_edmondsKarp(TEXT, ANYARRAY, ANYARRAY)
IS 'pgr_edmondsKarp(Many to Many)
- Directed graph
- Parameters:
  - Edges SQL with columns: id, source, target, capacity [,reverse_capacity]
  - From ARRAY[vertices identifiers]
  - to ARRAY[vertices identifiers]
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_edmondsKarp.html
';

COMMENT ON FUNCTION pgr_edmondsKarp(TEXT, TEXT)
IS 'pgr_edmondsKarp(Combinations)
- Directed graph
- Parameters:
  - Edges SQL with columns: id, source, target, capacity [,reverse_capacity]
  - Combinations SQL with columns: source, target
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_edmondsKarp.html
';


------------------------------------
-- pgr_boykovKolmogorov
------------------------------------


-- ONE to ONE
--v2.6
CREATE FUNCTION pgr_boykovKolmogorov(
    TEXT, -- edges_sql (required)
    BIGINT, -- from_vid (required)
    BIGINT, -- to_vid (required)

    OUT seq INTEGER,
    OUT edge BIGINT,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT flow BIGINT,
    OUT residual_capacity BIGINT)
  RETURNS SETOF RECORD AS
  $BODY$
        SELECT *
        FROM _pgr_maxflow(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], ARRAY[$3]::BIGINT[], 2);
  $BODY$
  LANGUAGE sql VOLATILE STRICT;


-- ONE to MANY
--v2.6
CREATE FUNCTION pgr_boykovKolmogorov(
    TEXT, -- edges_sql (required)
    BIGINT, -- from_vid (required)
    ANYARRAY, -- to_vids (required)

    OUT seq INTEGER,
    OUT edge BIGINT,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT flow BIGINT,
    OUT residual_capacity BIGINT)
  RETURNS SETOF RECORD AS
  $BODY$
        SELECT *
        FROM _pgr_maxflow(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], $3::BIGINT[], 2);
  $BODY$
  LANGUAGE sql VOLATILE STRICT;


-- MANY to ONE
--v2.6
CREATE FUNCTION pgr_boykovKolmogorov(
    TEXT, -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    BIGINT, -- to_vid (required)

    OUT seq INTEGER,
    OUT edge BIGINT,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT flow BIGINT,
    OUT residual_capacity BIGINT)
  RETURNS SETOF RECORD AS
  $BODY$
        SELECT *
        FROM _pgr_maxflow(_pgr_get_statement($1), $2::BIGINT[], ARRAY[$3]::BIGINT[], 2);
  $BODY$
  LANGUAGE sql VOLATILE STRICT;


-- MANY to MANY
--v2.6
CREATE FUNCTION pgr_boykovKolmogorov(
    TEXT, -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    ANYARRAY, -- to_vids (required)

    OUT seq INTEGER,
    OUT edge BIGINT,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT flow BIGINT,
    OUT residual_capacity BIGINT)
  RETURNS SETOF RECORD AS
  $BODY$
        SELECT *
        FROM _pgr_maxflow(_pgr_get_statement($1), $2::BIGINT[], $3::BIGINT[], 2);
  $BODY$
  LANGUAGE sql VOLATILE STRICT;


-- COMBINATIONS
--v3.2
CREATE FUNCTION pgr_boykovKolmogorov(
    TEXT, -- edges_sql (required)
    TEXT, -- combinations_sql (required)

    OUT seq INTEGER,
    OUT edge BIGINT,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT flow BIGINT,
    OUT residual_capacity BIGINT)
  RETURNS SETOF RECORD AS
  $BODY$
        SELECT *
        FROM _pgr_maxflow(_pgr_get_statement($1), _pgr_get_statement($2), 2);
  $BODY$
  LANGUAGE sql VOLATILE STRICT;


-- COMMENTS


COMMENT ON FUNCTION pgr_boykovKolmogorov(TEXT, BIGINT, BIGINT)
IS 'pgr_boykovKolmogorov(One to One)
- Directed graph
- Parameters:
  - edges SQL with columns: id, source, target, capacity [,reverse_capacity]
  - from vertex
  - to vertex
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_boykovKolmogorov.html
';


COMMENT ON FUNCTION pgr_boykovKolmogorov(TEXT, BIGINT, ANYARRAY)
IS 'pgr_boykovKolmogorov(One to Many)
- Directed graph
- Parameters:
  - edges SQL with columns: id, source, target, capacity [,reverse_capacity]
  - from vertex
  - to ARRAY[vertices identifiers]
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_boykovKolmogorov.html
';


COMMENT ON FUNCTION pgr_boykovKolmogorov(TEXT, ANYARRAY, BIGINT)
IS 'pgr_boykovKolmogorov(Many to One)
- Directed graph
- Parameters:
  - edges SQL with columns: id, source, target, capacity [,reverse_capacity]
  - from ARRAY[vertices identifiers]
  - to vertex
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_boykovKolmogorov.html
';


COMMENT ON FUNCTION pgr_boykovKolmogorov(TEXT, ANYARRAY, ANYARRAY)
IS 'pgr_boykovKolmogorov(Many to Many)
 - Directed graph
 - Parameters:
   - edges SQL with columns: id, source, target, capacity [,reverse_capacity]
   - from ARRAY[vertices identifiers]
   - to ARRAY[vertices identifiers]
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_boykovKolmogorov.html
';

COMMENT ON FUNCTION pgr_boykovKolmogorov(TEXT, TEXT)
IS 'pgr_boykovKolmogorov(Combinations)
 - Directed graph
 - Parameters:
   - Edges SQL with columns: id, source, target, capacity [,reverse_capacity]
   - Combinations SQL with columns: source, target
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_boykovKolmogorov.html
';


------------------------------------
-- pgr_pushRelabel
------------------------------------


-- ONE to ONE
--v2.6
CREATE FUNCTION pgr_pushRelabel(
    TEXT, -- edges_sql (required)
    BIGINT, -- from_vid (required)
    BIGINT, -- to_vid (required)

    OUT seq INTEGER,
    OUT edge BIGINT,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT flow BIGINT,
    OUT residual_capacity BIGINT)
  RETURNS SETOF RECORD AS
  $BODY$
        SELECT *
        FROM _pgr_maxflow(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], ARRAY[$3]::BIGINT[], 1);
  $BODY$
  LANGUAGE sql VOLATILE STRICT;


-- ONE to MANY
--v2.6
CREATE FUNCTION pgr_pushRelabel(
    TEXT, -- edges_sql (required)
    BIGINT, -- from_vid (required)
    ANYARRAY, -- to_vids (required)

    OUT seq INTEGER,
    OUT edge BIGINT,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT flow BIGINT,
    OUT residual_capacity BIGINT)
  RETURNS SETOF RECORD AS
  $BODY$
        SELECT *
        FROM _pgr_maxflow(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], $3::BIGINT[], 1);
  $BODY$
  LANGUAGE sql VOLATILE STRICT;


-- MANY to ONE
--v2.6
CREATE FUNCTION pgr_pushRelabel(
    TEXT, -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    BIGINT, -- to_vid (required)

    OUT seq INTEGER,
    OUT edge BIGINT,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT flow BIGINT,
    OUT residual_capacity BIGINT)
  RETURNS SETOF RECORD AS
  $BODY$
        SELECT *
        FROM _pgr_maxflow(_pgr_get_statement($1), $2::BIGINT[], ARRAY[$3]::BIGINT[], 1);
  $BODY$
  LANGUAGE sql VOLATILE STRICT;


-- MANY to MANY
--v2.6
CREATE FUNCTION pgr_pushRelabel(
    TEXT, -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    ANYARRAY, -- to_vids (required)

    OUT seq INTEGER,
    OUT edge BIGINT,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT flow BIGINT,
    OUT residual_capacity BIGINT)
  RETURNS SETOF RECORD AS
  $BODY$
        SELECT *
        FROM _pgr_maxflow(_pgr_get_statement($1), $2::BIGINT[], $3::BIGINT[], 1);
  $BODY$
  LANGUAGE sql VOLATILE STRICT;


-- COMBINATIONS
--v3.2
CREATE FUNCTION pgr_pushRelabel(
    TEXT, -- edges_sql (required)
    TEXT, -- combinations_sql (required)

    OUT seq INTEGER,
    OUT edge BIGINT,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT flow BIGINT,
    OUT residual_capacity BIGINT)
  RETURNS SETOF RECORD AS
  $BODY$
        SELECT *
        FROM _pgr_maxflow(_pgr_get_statement($1), _pgr_get_statement($2), 1);
  $BODY$
  LANGUAGE sql VOLATILE STRICT;


-- COMMENTS


COMMENT ON FUNCTION pgr_pushRelabel(TEXT, BIGINT, BIGINT)
IS 'pgr_pushRelabel(One to One)
- Directed graph
- Parameters:
  - Edges SQL with columns: id, source, target, capacity [,reverse_capacity]
  - From vertex identifier
  - To vertex identifier
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_pushRelabel.html
';


COMMENT ON FUNCTION pgr_pushRelabel(TEXT, BIGINT, ANYARRAY)
IS 'pgr_pushRelabel(One to Many)
- Directed graph
- Parameters:
  - Edges SQL with columns: id, source, target, capacity [,reverse_capacity]
  - From vertex identifie
  - To ARRAY[vertices identifiers]
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_pushRelabel.html
';


COMMENT ON FUNCTION pgr_pushRelabel(TEXT, ANYARRAY, BIGINT)
IS 'pgr_pushRelabel(Many to One)
- Directed graph
- Parameters:
  - Edges SQL with columns: id, source, target, capacity [,reverse_capacity]
  - From ARRAY[vertices identifiers]
  - To vertex identifie
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_pushRelabel.html
';


COMMENT ON FUNCTION pgr_pushRelabel(TEXT, ANYARRAY, ANYARRAY)
IS 'pgr_pushRelabel(Many to Many)
- Directed graph
- Parameters:
  - Edges SQL with columns: id, source, target, capacity [,reverse_capacity]
  - From ARRAY[vertices identifiers]
  - To ARRAY[vertices identifiers]
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_pushRelabel.html
';

COMMENT ON FUNCTION pgr_pushRelabel(TEXT, TEXT)
IS 'pgr_pushRelabel(Combinations)
- Directed graph
- Parameters:
  - Edges SQL with columns: id, source, target, capacity [,reverse_capacity]
  - Combinations SQL with columns: source, target
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_pushRelabel.html
';


--------------------------
-- pgr_maxCardinalityMatch
---------------------------


--v3.0
CREATE FUNCTION _pgr_maxCardinalityMatch(
    edges_sql TEXT,
    directed BOOLEAN,

    OUT seq INTEGER,
    OUT edge BIGINT,
    OUT source BIGINT,
    OUT target BIGINT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_maxCardinalityMatch(TEXT, BOOLEAN)
IS 'pgRouting internal function';


--v3.4
CREATE FUNCTION pgr_maxCardinalityMatch(
  TEXT, -- edges_sql (required)

  OUT edge BIGINT)
RETURNS SETOF BIGINT AS
$BODY$
SELECT edge
FROM _pgr_maxCardinalityMatch(_pgr_get_statement($1), false)
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;

--v3.4
CREATE FUNCTION pgr_maxCardinalityMatch(
  TEXT, -- edges_sql (required)

  directed BOOLEAN,

  OUT seq INTEGER,
  OUT edge BIGINT,
  OUT source BIGINT,
  OUT target BIGINT)
RETURNS SETOF RECORD AS
$BODY$
BEGIN
RAISE WARNING 'pgr_maxCardinalityMatch(text,boolean) deprecated on v3.4.0';
RETURN QUERY SELECT *
FROM _pgr_maxCardinalityMatch(_pgr_get_statement($1), $2);
END
$BODY$
LANGUAGE plpgsql VOLATILE STRICT
COST 100
ROWS 1000;

-- COMMENTS
COMMENT ON FUNCTION pgr_maxCardinalityMatch(TEXT, BOOLEAN)
IS 'pgr_maxCardinalityMatch
- DEPRECATED signature on v3.4.0
- Use without boolean paramater
- Regardless of boolen value it will work for undirected graphs
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_maxCardinalityMatch.html
';

COMMENT ON FUNCTION pgr_maxCardinalityMatch(TEXT)
IS 'pgr_maxCardinalityMatch
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
- Documentation:
- https://docs.pgrouting.org/latest/en/pgr_maxCardinalityMatch.html
';


------------------------
-- pgr_edgeDisjointPaths
------------------------

--v3.0
CREATE FUNCTION _pgr_edgeDisjointPaths(
    TEXT,
    ANYARRAY,
    ANYARRAY,

    directed BOOLEAN,

    OUT seq INTEGER,
    OUT path_id INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SEtoF RECORD AS
'MODULE_PATHNAME'
LANGUAGE C VOLATILE STRICT;

--v3.2
CREATE FUNCTION _pgr_edgeDisjointPaths(
    TEXT,
    TEXT,

    directed BOOLEAN,

    OUT seq INTEGER,
    OUT path_id INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SEtoF RECORD AS
'MODULE_PATHNAME'
LANGUAGE C VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_edgeDisjointPaths(TEXT, ANYARRAY, ANYARRAY, BOOLEAN)
IS 'pgRouting internal function';

COMMENT ON FUNCTION _pgr_edgeDisjointPaths(TEXT, TEXT, BOOLEAN)
IS 'pgRouting internal function';


-- ONE to ONE
--v2.6
CREATE FUNCTION pgr_edgeDisjointPaths(
    TEXT,   --edges_sql (required)
    BIGINT, -- From_vid (required)
    BIGINT, -- to_vid (required)

    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_id INTEGER,
    OUT path_seq INTEGER,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
  RETURNS SEtoF RECORD AS
  $BODY$
    SELECT a.seq, a.path_id, a.path_seq, a.node, a.edge, a.cost, a.agg_cost
    From _pgr_edgeDisjointPaths(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], ARRAY[$3]::BIGINT[], $4) AS a;
  $BODY$
LANGUAGE SQL VOLATILE STRICT;


-- ONE to MANY
--v2.6
CREATE FUNCTION pgr_edgeDisjointPaths(
    TEXT,     --edges_sql (required)
    BIGINT,   -- From_vid (required)
    ANYARRAY, -- to_vids (required)

    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_id INTEGER,
    OUT path_seq INTEGER,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT
    )
  RETURNS SEtoF RECORD AS
  $BODY$
    SELECT a.seq, a.path_id, a.path_seq, a.end_vid, a.node, a.edge, a.cost, a.agg_cost
    From _pgr_edgeDisjointPaths(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], $3::BIGINT[], $4) AS a;
  $BODY$
LANGUAGE SQL VOLATILE STRICT;


-- MANY to ONE
--v2.6
CREATE FUNCTION pgr_edgeDisjointPaths(
    TEXT,     --edges_sql (required)
    ANYARRAY, -- From_vids (required)
    BIGINT,   -- to_vid (required)

    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_id INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT
    )
  RETURNS SEtoF RECORD AS
  $BODY$
    SELECT a.seq, a.path_id, a.path_seq, a.start_vid, a.node, a.edge, a.cost, a.agg_cost
    From _pgr_edgeDisjointPaths(_pgr_get_statement($1), $2::BIGINT[], ARRAY[$3]::BIGINT[], $4) AS a;
  $BODY$
LANGUAGE SQL VOLATILE STRICT;


-- MANY to MANY
--v2.6
CREATE FUNCTION pgr_edgeDisjointPaths(
    TEXT,     --edges_sql (required)
    ANYARRAY, -- From_vids (required)
    ANYARRAY, -- to_vids (required)

    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_id INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
  RETURNS SEtoF RECORD AS
  $BODY$
    SELECT *
    From _pgr_edgeDisjointPaths(_pgr_get_statement($1), $2::BIGINT[], $3::BIGINT[], $4) AS a;
  $BODY$
LANGUAGE SQL VOLATILE STRICT;


-- COMBINATIONS
--v3.2
CREATE FUNCTION pgr_edgeDisjointPaths(
    TEXT,     -- edges_sql (required)
    TEXT,     -- combinations_sql (required)

    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_id INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
  RETURNS SEtoF RECORD AS
  $BODY$
    SELECT *
    From _pgr_edgeDisjointPaths(_pgr_get_statement($1), _pgr_get_statement($2), $3) AS a;
  $BODY$
LANGUAGE SQL VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION pgr_edgeDisjointPaths(TEXT, BIGINT, BIGINT, BOOLEAN)
IS 'pgr_edgeDisjointPaths(One to One)
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From vertex identifier
  - to vertex identifier
- Optional Parameters
   - directed := true
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_edgeDisjointPaths.html
';


COMMENT ON FUNCTION pgr_edgeDisjointPaths(TEXT, BIGINT, ANYARRAY, BOOLEAN)
IS 'pgr_edgeDisjointPaths(One to Many)
 - Parameters:
   - dges SQL with columns: id, source, target, cost [,reverse_cost]
   - From vertex identifier
   - to ARRAY[vertices identifiers]
- Optional Parameters
   - directed := true
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_edgeDisjointPaths.html
';


COMMENT ON FUNCTION pgr_edgeDisjointPaths(TEXT, ANYARRAY, BIGINT, BOOLEAN)
IS 'pgr_edgeDisjointPaths(Many to One)
 - Parameters:
   - edges SQL with columns: id, source, target, cost [,reverse_cost]
   - From ARRAY[vertices identifiers]
   - to vertex identifier
- Optional Parameters
   - directed := true
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_edgeDisjointPaths.html
';


COMMENT ON FUNCTION pgr_edgeDisjointPaths(TEXT, ANYARRAY, ANYARRAY, BOOLEAN)
IS 'pgr_edgeDisjointPaths(Many to Many)
 - Parameters:
   - edges SQL with columns: id, source, target, cost [,reverse_cost]
   - From ARRAY[vertices identifiers]
   - to ARRAY[vertices identifiers]
- Optional Parameters
   - directed := true
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_edgeDisjointPaths.html
';

COMMENT ON FUNCTION pgr_edgeDisjointPaths(TEXT, TEXT, BOOLEAN)
IS 'pgr_edgeDisjointPaths(Combinations)
 - Parameters:
   - Edges SQL with columns: id, source, target, cost [,reverse_cost]
   - Combinations SQL with columns: source, target
- Optional Parameters
   - directed := true
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_edgeDisjointPaths.html
';


------------------------
-- pgr_maxFlowMinCost
------------------------

--    ONE TO ONE
--v3.0
CREATE FUNCTION pgr_maxFlowMinCost(
    TEXT,   -- edges_sql (required)
    BIGINT,   -- source (required)
    BIGINT,   -- target (required)

    OUT seq INTEGER,
    OUT edge BIGINT,
    OUT source BIGINT,
    OUT target BIGINT,
    OUT flow BIGINT,
    OUT residual_capacity BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_maxFlowMinCost(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], ARRAY[$3]::BIGINT[], only_cost := false);
$BODY$
LANGUAGE SQL VOLATILE STRICT;


--    ONE TO MANY
--v3.0
CREATE FUNCTION pgr_maxFlowMinCost(
    TEXT,   -- edges_sql (required)
    BIGINT, -- sources (required)
    ANYARRAY,   -- target (required)

    OUT seq INTEGER,
    OUT edge BIGINT,
    OUT source BIGINT,
    OUT target BIGINT,
    OUT flow BIGINT,
    OUT residual_capacity BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_maxFlowMinCost(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], $3::BIGINT[], only_cost := false);
$BODY$
LANGUAGE SQL VOLATILE STRICT;


--    MANY TO ONE
--v3.0
CREATE FUNCTION pgr_maxFlowMinCost(
    TEXT,   -- edges_sql (required)
    ANYARRAY,   -- source (required)
    BIGINT, -- targets (required)

    OUT seq INTEGER,
    OUT edge BIGINT,
    OUT source BIGINT,
    OUT target BIGINT,
    OUT flow BIGINT,
    OUT residual_capacity BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_maxFlowMinCost(_pgr_get_statement($1), $2::BIGINT[], ARRAY[$3]::BIGINT[], only_cost := false);
$BODY$
LANGUAGE SQL VOLATILE STRICT;

--    MANY TO MANY
--v3.0
CREATE FUNCTION pgr_maxFlowMinCost(
    TEXT,   -- edges_sql (required)
    ANYARRAY, -- sources (required)
    ANYARRAY, -- targets (required)

    OUT seq INTEGER,
    OUT edge BIGINT,
    OUT source BIGINT,
    OUT target BIGINT,
    OUT flow BIGINT,
    OUT residual_capacity BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_maxFlowMinCost(_pgr_get_statement($1), $2::BIGINT[], $3::BIGINT[], only_cost := false);
$BODY$
LANGUAGE SQL VOLATILE STRICT;


-- COMBINATIONS
--v3.0
CREATE FUNCTION pgr_maxFlowMinCost(
    TEXT,   -- edges_sql (required)
    TEXT,   -- combinations_sql (required)

    OUT seq INTEGER,
    OUT edge BIGINT,
    OUT source BIGINT,
    OUT target BIGINT,
    OUT flow BIGINT,
    OUT residual_capacity BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_maxFlowMinCost(_pgr_get_statement($1), _pgr_get_statement($2), only_cost := false);
$BODY$
LANGUAGE SQL VOLATILE STRICT;


-- COMMENTS

COMMENT ON FUNCTION pgr_maxFlowMinCost(TEXT, BIGINT, BIGINT)
IS 'pgr_maxFlowMinCost(One to One)
- EXPERIMENTAL
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From vertex identifier
  - To vertex identifier
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_maxFlowMinCost.html
';

COMMENT ON FUNCTION pgr_maxFlowMinCost(TEXT, BIGINT, ANYARRAY)
IS 'pgr_maxFlowMinCost(One to Many)
- EXPERIMENTAL
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From vertex identifier
  - To ARRAY[vertices identifiers]
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_maxFlowMinCost.html
';

COMMENT ON FUNCTION pgr_maxFlowMinCost(TEXT, ANYARRAY, BIGINT)
IS 'pgr_maxFlowMinCost(Many to One)
- EXPERIMENTAL
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From ARRAY[vertices identifiers]vertex identifier
  - To vertex identifier
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_maxFlowMinCost.html
';

COMMENT ON FUNCTION pgr_maxFlowMinCost(TEXT, ANYARRAY, ANYARRAY)
IS 'EXPERIMENTAL pgr_maxFlowMinCost(Many to Many)
- EXPERIMENTAL
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From ARRAY[vertices identifiers]
  - To ARRAY[vertices identifiers]
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_maxFlowMinCost.html
';

COMMENT ON FUNCTION pgr_maxFlowMinCost(TEXT, TEXT)
IS 'EXPERIMENTAL pgr_maxFlowMinCost(Combinations)
- EXPERIMENTAL
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - Combinations SQL with columns: source, target
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_maxFlowMinCost.html
';


----------------------------
-- pgr_maxFlowMinCost_Cost
----------------------------

--    ONE TO ONE
--v3.0
CREATE FUNCTION pgr_maxFlowMinCost_Cost(
    TEXT,   -- edges_sql (required)
    BIGINT,   -- source (required)
    BIGINT)   -- target (required)

RETURNS FLOAT AS
$BODY$
    SELECT cost
    FROM _pgr_maxFlowMinCost(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], ARRAY[$3]::BIGINT[], only_cost := true);
$BODY$
LANGUAGE SQL VOLATILE STRICT;


--    ONE TO MANY
--v3.0
CREATE FUNCTION pgr_maxFlowMinCost_Cost(
    TEXT,   -- edges_sql (required)
    BIGINT,   -- source (required)
    ANYARRAY) -- targets (required)

RETURNS FLOAT AS
$BODY$
    SELECT cost
    FROM _pgr_maxFlowMinCost(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], $3::BIGINT[], only_cost := true);
$BODY$
LANGUAGE SQL VOLATILE STRICT;


--    MANY TO ONE
--v3.0
CREATE FUNCTION pgr_maxFlowMinCost_Cost(
    TEXT,   -- edges_sql (required)
    ANYARRAY, -- sources (required)
    BIGINT)   -- target (required)

RETURNS FLOAT AS
$BODY$
    SELECT cost
    FROM _pgr_maxFlowMinCost(_pgr_get_statement($1), $2::BIGINT[], ARRAY[$3]::BIGINT[], only_cost := true);
$BODY$
LANGUAGE SQL VOLATILE STRICT;


--    MANY TO MANY
--v3.0
CREATE FUNCTION pgr_maxFlowMinCost_Cost(
    TEXT,   -- edges_sql (required)
    ANYARRAY, -- sources (required)
    ANYARRAY) -- targets (required)

RETURNS FLOAT AS
$BODY$
    SELECT cost
    FROM _pgr_maxFlowMinCost(_pgr_get_statement($1), $2::BIGINT[], $3::BIGINT[], only_cost := true);
$BODY$
LANGUAGE SQL VOLATILE STRICT;


-- COMBINATIONS
--v3.2
CREATE FUNCTION pgr_maxFlowMinCost_Cost(
    TEXT,   -- edges_sql (required)
    TEXT)   -- combinations_sql (required)

RETURNS FLOAT AS
$BODY$
    SELECT cost
    FROM _pgr_maxFlowMinCost(_pgr_get_statement($1), _pgr_get_statement($2), only_cost := true);
$BODY$
LANGUAGE SQL VOLATILE STRICT;


-- COMMENTS

COMMENT ON FUNCTION pgr_maxFlowMinCost_Cost(TEXT, BIGINT, BIGINT)
IS 'pgr_maxFlowMinCost_Cost (One to One)
- EXPERIMENTAL
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From vertex identifier
  - To vertex identifier
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_maxFlowMinCost_Cost.html
';

COMMENT ON FUNCTION pgr_maxFlowMinCost_Cost(TEXT, BIGINT, ANYARRAY)
IS 'pgr_maxFlowMinCost_Cost(One to Many)
- EXPERIMENTAL
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From vertex identifier
  - To ARRAY[vertices identifiers]
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_maxFlowMinCost_Cost.html
  ';

COMMENT ON FUNCTION pgr_maxFlowMinCost_Cost(TEXT, ANYARRAY, BIGINT)
IS 'pgr_maxFlowMinCost_Cost (Many to One)
- EXPERIMENTAL
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From ARRAY[vertices identifiers]
  - To vertex identifier
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_maxFlowMinCost_Cost.html
';

COMMENT ON FUNCTION pgr_maxFlowMinCost_Cost(TEXT, ANYARRAY, ANYARRAY)
IS 'EXPERIMENTAL pgr_maxFlowMinCost_Cost (Many to Many)
- EXPERIMENTAL
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From ARRAY[vertices identifiers]
  - To ARRAY[vertices identifiers]
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_maxFlowMinCost_Cost.html
';

COMMENT ON FUNCTION pgr_maxFlowMinCost_Cost(TEXT, TEXT)
IS 'EXPERIMENTAL pgr_maxFlowMinCost_Cost (Combinations)
- EXPERIMENTAL
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - Combinations SQL with columns: source, target
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_maxFlowMinCost_Cost.html
';


---------------
-- pgr_maxFlow
---------------


-- ONE to ONE
--v2.6
CREATE FUNCTION pgr_maxFlow(
    TEXT, -- edges_sql (required)
    BIGINT, -- from_vid (required)
    BIGINT) -- to_vid (required)
  RETURNS BIGINT AS
  $BODY$
        SELECT flow
        FROM _pgr_maxflow(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], ARRAY[$3]::BIGINT[], algorithm := 1, only_flow := true);
  $BODY$
  LANGUAGE sql VOLATILE STRICT;


-- ONE to MANY
--v2.6
CREATE FUNCTION pgr_maxFlow(
    TEXT, -- edges_sql (required)
    BIGINT, -- from_vid (required)
    ANYARRAY) -- to_vids (required)
  RETURNS BIGINT AS
  $BODY$
        SELECT flow
        FROM _pgr_maxflow(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], $3::BIGINT[], algorithm := 1, only_flow := true);
  $BODY$
  LANGUAGE sql VOLATILE STRICT;


-- MANY to ONE
--v2.6
CREATE FUNCTION pgr_maxFlow(
    TEXT, -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    BIGINT) -- to_vid (required)
  RETURNS BIGINT AS
  $BODY$
        SELECT flow
        FROM _pgr_maxflow(_pgr_get_statement($1), $2::BIGINT[], ARRAY[$3]::BIGINT[], algorithm := 1, only_flow := true);
  $BODY$
  LANGUAGE sql VOLATILE STRICT;


-- MANY to MANY
--v2.6
CREATE FUNCTION pgr_maxFlow(
    TEXT, -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    ANYARRAY) -- to_vids (required)
  RETURNS BIGINT AS
  $BODY$
        SELECT flow
        FROM _pgr_maxflow(_pgr_get_statement($1), $2::BIGINT[], $3::BIGINT[], algorithm := 1, only_flow := true);
  $BODY$
  LANGUAGE sql VOLATILE STRICT;


-- COMBINATIONS
--v3.2
CREATE FUNCTION pgr_maxFlow(
    TEXT, -- edges_sql (required)
    TEXT) -- combinations_sql (required)
  RETURNS BIGINT AS
  $BODY$
        SELECT flow
        FROM _pgr_maxflow(_pgr_get_statement($1), _pgr_get_statement($2), algorithm := 1, only_flow := true);
  $BODY$
  LANGUAGE sql VOLATILE STRICT;


-- COMMENTS


COMMENT ON FUNCTION pgr_maxFlow(TEXT, BIGINT, BIGINT)
IS 'pgr_maxFlow(One to One)
- Directed graph
- Parameters:
  - edges SQL with columns: id, source, target, capacity [,reverse_capacity]
  - from vertex
  - to vertex
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_maxFlow.html
';

COMMENT ON FUNCTION pgr_maxFlow(TEXT, BIGINT, ANYARRAY)
IS 'pgr_maxFlow(One to Many)
- Directed graph
- Parameters:
  - edges SQL with columns: id, source, target, capacity [,reverse_capacity]
  - from vertex
  - to ARRAY[vertices identifiers]
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_maxFlow.html
';

COMMENT ON FUNCTION pgr_maxFlow(TEXT, ANYARRAY, BIGINT)
IS 'pgr_maxFlow(Many to One)
- Directed graph
- Parameters:
  - edges SQL with columns: id, source, target, capacity [,reverse_capacity]
  - from ARRAY[vertices identifiers]
  - to vertex
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_maxFlow.html
';

COMMENT ON FUNCTION pgr_maxFlow(TEXT, ANYARRAY, ANYARRAY)
IS 'pgr_maxFlow(Many to Many)
- Directed graph
- Parameters:
  - edges SQL with columns: id, source, target, capacity [,reverse_capacity]
  - from ARRAY[vertices identifiers]
  - to ARRAY[vertices identifiers]
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_maxFlow.html
';

COMMENT ON FUNCTION pgr_maxFlow(TEXT, TEXT)
IS 'pgr_maxFlow(Combinations)
- Directed graph
- Parameters:
  - Edges SQL with columns: id, source, target, capacity [,reverse_capacity]
  - Combinations SQL with columns: source, target
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_maxFlow.html
';


--------------------
--------------------
-- contraction
--------------------
--------------------

--v3.0
CREATE FUNCTION _pgr_contraction(
    edges_sql TEXT,
    contraction_order BIGINT[],
    max_cycles INTEGER DEFAULT 1,
    forbidden_vertices BIGINT[] DEFAULT ARRAY[]::BIGINT[],
    directed BOOLEAN DEFAULT true,

    OUT type TEXT,
    OUT id BIGINT,
    OUT contracted_vertices BIGINT[],
    OUT source BIGINT,
    OUT target BIGINT,
    OUT cost FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE C VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_contraction(TEXT, BIGINT[], INTEGER, BIGINT[], BOOLEAN)
IS 'pgRouting internal function';


--------------------
--------------------
-- contraction
--------------------
--------------------

--------------------
-- pgr_contraction
--------------------


--v3.0
CREATE FUNCTION pgr_contraction(
    TEXT,     -- edges_sql (required)
    BIGINT[], -- contraction_order (required)

    max_cycles INTEGER DEFAULT 1,
    forbidden_vertices BIGINT[] DEFAULT ARRAY[]::BIGINT[],
    directed BOOLEAN DEFAULT true,

    OUT type TEXT,
    OUT id BIGINT,
    OUT contracted_vertices BIGINT[],
    OUT source BIGINT,
    OUT target BIGINT,
    OUT cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_contraction(_pgr_get_statement($1), $2::BIGINT[],  $3, $4, $5);
$BODY$
LANGUAGE SQL VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION pgr_contraction(TEXT, BIGINT[], INTEGER, BIGINT[], BOOLEAN)
IS 'pgr_contraction
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - ARRAY [Contraction order]
- Optional Parameters
    - max_cycles := 1
    - forbidden_vertices := ARRAY[]::BIGINT[]
    - directed := true
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_contraction.html
';


--v2.6
CREATE FUNCTION _pgr_pickDeliver(
    TEXT, -- orders_sql
    TEXT, -- vehicles_sql
    TEXT, -- matrix_cell_sql

    factor FLOAT DEFAULT 1,
    max_cycles INTEGER DEFAULT 10,
    initial_sol INTEGER DEFAULT 4,

    OUT seq INTEGER,
    OUT vehicle_seq INTEGER,
    OUT vehicle_id BIGINT,
    OUT stop_seq INTEGER,
    OUT stop_type INTEGER,
    OUT stop_id BIGINT,
    OUT order_id BIGINT,
    OUT cargo FLOAT,
    OUT travel_time FLOAT,
    OUT arrival_time FLOAT,
    OUT wait_time FLOAT,
    OUT service_time FLOAT,
    OUT departure_time FLOAT)
RETURNS SETOF RECORD AS
 'MODULE_PATHNAME'
LANGUAGE c VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_pickDeliver(TEXT, TEXT, TEXT, FLOAT, INTEGER, INTEGER)
IS 'pgRouting internal function';


--v3.0
CREATE FUNCTION pgr_pickDeliver(
    TEXT, -- orders_sql (required)
    TEXT, -- vehicles_sql (required)
    TEXT, -- matrix_cell_sql

    factor FLOAT DEFAULT 1,
    max_cycles INTEGER DEFAULT 10,
    initial_sol INTEGER DEFAULT 4,

    OUT seq INTEGER,
    OUT vehicle_seq INTEGER,
    OUT vehicle_id BIGINT,
    OUT stop_seq INTEGER,
    OUT stop_type INTEGER,
    OUT stop_id BIGINT,
    OUT order_id BIGINT,
    OUT cargo FLOAT,
    OUT travel_time FLOAT,
    OUT arrival_time FLOAT,
    OUT wait_time FLOAT,
    OUT service_time FLOAT,
    OUT departure_time FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_pickDeliver(_pgr_get_statement($1), _pgr_get_statement($2), $3, $4, $5);
$BODY$
LANGUAGE SQL VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION pgr_pickDeliver(TEXT, TEXT, TEXT, FLOAT, INTEGER, INTEGER)
IS 'pgr_pickDeliver
 - EXPERIMENTAL
 - Parameters:
   - orders SQL with columns:
     - id, demand, p_node_id, p_open, p_close, d_node_id, d_open, d_close
     - optional columns:
        - p_service := 0
        - d_service := 0
   - vehicles SQL with columns:
     - id, capacity, start_open, start_close
     - optional columns:
        - speed := 1
        - start_service := 0
        - end_open := start_open
        - end_close := start_close
        - end_service := 0
   - Matrix
     - start_vid
     - end_vid
     - agg_cost
 - Optional Parameters:
   - factor: default := 1
   - max_cycles: default := 10
   - initial_sol: default := 4
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_pickDeliver.html
';


--v2.6
CREATE FUNCTION _pgr_pickDeliverEuclidean (
    TEXT, -- orders_sql
    TEXT, -- vehicles_sql

    factor FLOAT DEFAULT 1,
    max_cycles INTEGER DEFAULT 10,
    initial_sol INTEGER DEFAULT 4,

    OUT seq INTEGER,
    OUT vehicle_seq INTEGER,
    OUT vehicle_id BIGINT,
    OUT stop_seq INTEGER,
    OUT stop_type INTEGER,
    OUT order_id BIGINT,
    OUT cargo FLOAT,
    OUT travel_time FLOAT,
    OUT arrival_time FLOAT,
    OUT wait_time FLOAT,
    OUT service_time FLOAT,
    OUT departure_time FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_pickDeliverEuclidean(TEXT, TEXT, FLOAT, INTEGER, INTEGER)
IS 'pgRouting internal function';


--v3.0
CREATE FUNCTION pgr_pickDeliverEuclidean(
    TEXT, -- orders_sql (required)
    TEXT, -- vehicles_sql (required)

    factor FLOAT DEFAULT 1,
    max_cycles INTEGER DEFAULT 10,
    initial_sol INTEGER DEFAULT 4,

    OUT seq INTEGER,
    OUT vehicle_seq INTEGER,
    OUT vehicle_id BIGINT,
    OUT stop_seq INTEGER,
    OUT stop_type INTEGER,
    OUT order_id BIGINT,
    OUT cargo FLOAT,
    OUT travel_time FLOAT,
    OUT arrival_time FLOAT,
    OUT wait_time FLOAT,
    OUT service_time FLOAT,
    OUT departure_time FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_pickDeliverEuclidean(_pgr_get_statement($1), _pgr_get_statement($2), $3, $4, $5);
$BODY$
LANGUAGE SQL VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION pgr_pickDeliverEuclidean(TEXT, TEXT, FLOAT, INTEGER, INTEGER)
IS 'pgr_pickDeliverEuclidean
 - EXPERIMENTAL
 - Parameters:
   - orders SQL with columns:
     - id, demand, p_x, p_t, d_x, d_y, p_open, p_close, d_open, d_close
     - optional columns:
       - p_service := 0
       - d_service := 0
   - vehicles SQL with columns:
     - id, start_x, start_y, capacity, start_open, start_close
     - optional columns:
       - speed := 1
       - start_service := 0
       - end_x := start_x
       - end_y := start_y
       - end_open := start_open
       - end_close := start_close
       - end_service := 0
 - Optional Parameters:
   - factor: default := 1
   - max_cycles: default := 10
   - initial_sol: default := 4
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_pickDeliver.html
';


--------------------
-- _pgr_vrpOneDepot
--------------------


--v2.6
CREATE FUNCTION _pgr_vrpOneDepot(
    TEXT, -- customers_sql
    TEXT, -- vehicles_sql
    TEXT, -- matrix_sql
    INTEGER, -- depot_id

    OUT seq INTEGER,
    OUT vehicle_seq INTEGER,
    OUT vehicle_id BIGINT,
    OUT stop_seq INTEGER,
    OUT stop_type INTEGER,
    OUT stop_id BIGINT,
    OUT order_id BIGINT,
    OUT cargo FLOAT,
    OUT travel_time FLOAT,
    OUT arrival_time FLOAT,
    OUT wait_time FLOAT,
    OUT service_time FLOAT,
    OUT departure_time FLOAT
)
RETURNS SETOF RECORD AS
$BODY$
DECLARE
orders_sql TEXT;
trucks_sql TEXT;
matrix_sql TEXT;
final_sql TEXT;
BEGIN

    orders_sql = $$WITH
    vrp_orders AS ($$ || $1 || $$ ),
    pickups AS (
        SELECT id, x AS p_x, y AS p_y, open_time AS p_open, close_time AS p_close, service_time AS p_service
        FROM vrp_orders
        WHERE id = $$ || $4 || $$
    )
    SELECT vrp_orders.id AS id, order_unit AS demand, pickups.id AS p_node_id, p_x, p_y, p_open, p_close, p_service,
    vrp_orders.id AS d_node_id, x AS d_x, y AS d_y, open_time AS d_open, close_time AS d_close, service_time AS d_service
    FROM vrp_orders, pickups
    WHERE vrp_orders.id != $$ || $4;


    trucks_sql = $$ WITH
    vrp_orders AS ($$ || $1 || $$ ),
    vrp_vehicles AS ($$ || $2 || $$ ),
    starts AS (
        SELECT id AS start_node_id, x AS start_x, y AS start_y, open_time AS start_open, close_time AS start_close, service_time AS start_service
        FROM vrp_orders
        WHERE id = $$ || $4 || $$
    )
    SELECT vehicle_id AS id, capacity, starts.* FROM vrp_vehicles, starts;
    $$;

    final_sql = '
    SELECT * FROM _pgr_pickDeliver(
            $$' || orders_sql || '$$,
            $$' || trucks_sql || '$$,
            $$' || $3 || '$$,
            max_cycles := 3,
            initial_sol := 7 ); ';

    RAISE DEBUG '%', orders_sql;
    RAISE DEBUG '%', trucks_sql;
    RAISE DEBUG '%', $3;
    RAISE DEBUG '%', final_sql;

    RETURN QUERY EXECUTE final_sql;
END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;


-- COMMENTS


COMMENT ON FUNCTION _pgr_vrpOneDepot(TEXT, TEXT, TEXT, INTEGER)
IS 'pgRouting internal function';

-----------------------------------------------------------------------
-- Core function for vrp with sigle depot computation
-- See README for description
-----------------------------------------------------------------------


--------------------
-- pgr_vrpOneDepot
--------------------


--v2.6
CREATE FUNCTION pgr_vrpOneDepot(
	text,  -- order_sql
	text, -- vehicle_sql
	text, -- cost_sql
	integer, -- depot_id

	OUT oid integer,
	OUT opos integer,
	OUT vid integer,
	OUT tarrival integer,
	OUT tdepart integer)
RETURNS SETOF RECORD AS
$BODY$
    SELECT order_id::INTEGER, stop_seq::INTEGER, vehicle_id::INTEGER, arrival_time::INTEGER, departure_time::INTEGER
    FROM _pgr_vrpOneDepot($1, $2,
       '
            SELECT src_id AS start_vid, dest_id AS end_vid, traveltime AS agg_cost FROM ('||$3||') AS a
       ',
       $4);
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 1000
ROWS 1000;


-- COMMENTS


COMMENT ON FUNCTION pgr_vrpOneDepot(TEXT, TEXT, TEXT, INTEGER)
IS 'pgr_vrpOneDepot
- EXPERIMENTAL
- Parameters
  - orders SQL with columns: id, x, y, order_unit, open_time, close_time, service_time
  - vehicle SQL with columns: vehicle_id, capacity, case_no
  - cost SQL with columns: src_id, dest_id, cost, distance, traveltime
  - depot id
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_vrpOneDepot.html
';



------------------
------------------
-- withPoints
------------------
------------------


------------------
-- _pgr_withPoints
------------------


--v2.6
CREATE FUNCTION _pgr_withPoints(
    edges_sql TEXT,
    points_sql TEXT,
    start_pids ANYARRAY,
    end_pids ANYARRAY,
    directed BOOLEAN,
    driving_side CHAR,
    details BOOLEAN,

    only_cost BOOLEAN DEFAULT false, -- gets path
    normal BOOLEAN DEFAULT true, -- false for many to onu


    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_pid BIGINT,
    OUT end_pid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c VOLATILE;


--v3.2
CREATE FUNCTION _pgr_withPoints(
    edges_sql TEXT,
    points_sql TEXT,
    combinations_sql TEXT,

    directed BOOLEAN,
    driving_side CHAR,
    details BOOLEAN,

    only_cost BOOLEAN DEFAULT false,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_pid BIGINT,
    OUT end_pid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE C VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_withPoints(TEXT, TEXT, ANYARRAY, ANYARRAY, BOOLEAN, CHAR, BOOLEAN, BOOLEAN, BOOLEAN)
IS 'pgRouting internal function';

COMMENT ON FUNCTION _pgr_withPoints(TEXT, TEXT, TEXT, BOOLEAN, CHAR, BOOLEAN, BOOLEAN)
IS 'pgRouting internal function';



--------------------
-- pgr_withPoints
--------------------


-- ONE TO ONE
--v2.6
CREATE FUNCTION pgr_withPoints(
    TEXT, -- edges_sql (required)
    TEXT, -- points_sql (required)
    BIGINT, -- end_pid (required)
    BIGINT, -- end_pid (required)


    directed BOOLEAN DEFAULT true,
    driving_side CHAR DEFAULT 'b', -- 'r'/'l'/'b'/NULL
    details BOOLEAN DEFAULT false,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_withPoints(_pgr_get_statement($1), $2, ARRAY[$3]::bigint[], ARRAY[$4]::bigint[], $5, $6, $7) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- ONE TO MANY
--v2.6
CREATE FUNCTION pgr_withPoints(
    TEXT, -- edges_sql (required)
    TEXT, -- points_sql (required)
    BIGINT, -- end_pid (required)
    ANYARRAY, -- end_pid (required)

    directed BOOLEAN DEFAULT true,
    driving_side CHAR DEFAULT 'b', -- 'r'/'l'/'b'/NULL
    details BOOLEAN DEFAULT false,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT end_pid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
SELECT a.seq, a.path_seq, a.end_pid, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_withPoints(_pgr_get_statement($1), $2, ARRAY[$3]::bigint[], $4::bigint[], $5, $6, $7) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- MANY TO ONE
--v2.6
CREATE FUNCTION pgr_withPoints(
    TEXT, -- edges_sql (required)
    TEXT, -- points_sql (required)
    ANYARRAY, -- end_pid (required)
    BIGINT, -- end_pid (required)

    directed BOOLEAN DEFAULT true,
    driving_side CHAR DEFAULT 'b', -- 'r'/'l'/'b'/NULL
    details BOOLEAN DEFAULT false,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_pid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
SELECT a.seq, a.path_seq, a.start_pid, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_withPoints(_pgr_get_statement($1), $2, $3::bigint[], ARRAY[$4]::bigint[], $5, $6, $7, FALSE, FALSE) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- MANY TO MANY
--v2.6
CREATE FUNCTION pgr_withPoints(
    TEXT, -- edges_sql (required)
    TEXT, -- points_sql (required)
    ANYARRAY, -- end_pid (required)
    ANYARRAY, -- end_pid (required)

    directed BOOLEAN DEFAULT true,
    driving_side CHAR DEFAULT 'b', -- 'r'/'l'/'b'/NULL
    details BOOLEAN DEFAULT false,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_pid BIGINT,
    OUT end_pid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
SELECT a.seq, a.path_seq, a.start_pid, a.end_pid, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_withPoints(_pgr_get_statement($1), $2, $3::bigint[], $4::bigint[], $5, $6, $7) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- Combinations SQL signature
--v3.2
CREATE FUNCTION pgr_withPoints(
    TEXT,     -- edges_sql (required)
    TEXT,     -- points_sql (required)
    TEXT,     -- combinations_sql (required)

    directed BOOLEAN DEFAULT true,
    driving_side CHAR DEFAULT 'b', -- 'r'/'l'/'b'/NULL
    details BOOLEAN DEFAULT false,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_pid BIGINT,
    OUT end_pid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.start_pid, a.end_pid, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_withPoints(_pgr_get_statement($1), _pgr_get_statement($2), _pgr_get_statement($3),
        $4, $5, $6) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- COMMENTS


COMMENT ON FUNCTION pgr_withPoints(TEXT, TEXT, BIGINT, BIGINT, BOOLEAN, CHAR, BOOLEAN)
IS 'pgr_withPoints (One to One)
- PROPOSED
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - Points SQL with columns: [pid], edge_id, fraction[,side]
    - From vertex identifier/point identifier
    - To vertex identifier/point identifier
- Optional Parameters
    - directed := ''true''
    - driving_side := ''b''
    - details := ''false''
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_withPoints.html
';


COMMENT ON FUNCTION pgr_withPoints(TEXT, TEXT, BIGINT, ANYARRAY, BOOLEAN, CHAR, BOOLEAN)
IS 'pgr_withPoints (One to Many)
- PROPOSED
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - Points SQL with columns: [pid], edge_id, fraction[,side]
    - From vertex identifier/point identifier
    - To ARRAY[vertices/points identifier]
- Optional Parameters
    - directed := ''true''
    - driving_side := ''b''
    - details := ''false''
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_withPoints.html
';


COMMENT ON FUNCTION pgr_withPoints(TEXT, TEXT, ANYARRAY, BIGINT, BOOLEAN, CHAR, BOOLEAN)
IS 'pgr_withPoints (Many to One)
- PROPOSED
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - Points SQL with columns: [pid], edge_id, fraction[,side]
    - From  ARRAY[vertices/points identifiers]
    - To vertex identifier/point identifier
- Optional Parameters
    - directed := ''true''
    - driving_side := ''b''
    - details := ''false''
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_withPoints.html
';


COMMENT ON FUNCTION pgr_withPoints(TEXT, TEXT, ANYARRAY, ANYARRAY, BOOLEAN, CHAR, BOOLEAN)
IS 'pgr_withPoints (Many to Many)
- PROPOSED
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - Points SQL with columns: [pid], edge_id, fraction[,side]
    - From ARRAY[vertices/points identifiers]
    - To ARRAY[vertices/points identifiers]
- Optional Parameters
    - directed := ''true''
    - driving_side := ''b''
    - details := ''false''
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_withPoints.html
';

COMMENT ON FUNCTION pgr_withPoints(TEXT, TEXT, TEXT, BOOLEAN, CHAR, BOOLEAN)
IS 'pgr_withPoints(Combinations)
- Parameters:
   - Edges SQL with columns: id, source, target, cost [,reverse_cost]
   - Points SQL with columns: [pid], edge_id, fraction [,side]
   - Combinations SQL with columns: source, target
- Optional Parameters
    - directed := ''true''
    - driving_side := ''b''
    - details := ''false''
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_withPoints.html
';



----------------------
-- pgr_withPointsCost
----------------------


-- ONE TO ONE
--v2.6
CREATE FUNCTION pgr_withPointsCost(
    TEXT, -- edges_sql (required)
    TEXT, -- points_sql (required)
    BIGINT, -- end_pid (required)
    BIGINT, -- end_pid (required)

    directed BOOLEAN DEFAULT true,
    driving_side CHAR DEFAULT 'b', -- 'r'/'l'/'b'/NULL

    OUT start_pid BIGINT,
    OUT end_pid BIGINT,
    OUT agg_cost float)
RETURNS SETOF RECORD AS
$BODY$
    SELECT $3, $4, a.agg_cost
    FROM _pgr_withPoints(_pgr_get_statement($1), $2, ARRAY[$3]::BIGINT[], ARRAY[$4]::BIGINT[], $5, $6, TRUE, TRUE) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- ONE TO MANY
--v2.6
CREATE FUNCTION pgr_withPointsCost(
    TEXT, -- edges_sql (required)
    TEXT, -- points_sql (required)
    BIGINT, -- end_pid (required)
    ANYARRAY, -- end_pid (required)

    directed BOOLEAN DEFAULT true,
    driving_side CHAR DEFAULT 'b', -- 'r'/'l'/'b'/NULL

    OUT start_pid BIGINT,
    OUT end_pid BIGINT,
    OUT agg_cost float)
RETURNS SETOF RECORD AS
$BODY$
    SELECT $3, a.end_pid, a.agg_cost
    FROM _pgr_withPoints(_pgr_get_statement($1), $2, ARRAY[$3]::BIGINT[], $4::BIGINT[], $5, $6, TRUE, TRUE) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- MANY TO ONE
--v2.6
CREATE FUNCTION pgr_withPointsCost(
    TEXT, -- edges_sql (required)
    TEXT, -- points_sql (required)
    ANYARRAY, -- end_pid (required)
    BIGINT, -- end_pid (required)

    directed BOOLEAN DEFAULT true,
    driving_side CHAR DEFAULT 'b', -- 'r'/'l'/'b'/NULL

    OUT start_pid BIGINT,
    OUT end_pid BIGINT,
    OUT agg_cost float)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.start_pid, $4, a.agg_cost
    FROM _pgr_withPoints(_pgr_get_statement($1), $2, $3::BIGINT[], ARRAY[$4]::BIGINT[], $5, $6, TRUE, TRUE) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- MANY TO MANY
--v2.6
CREATE FUNCTION pgr_withPointsCost(
    TEXT, -- edges_sql (required)
    TEXT, -- points_sql (required)
    ANYARRAY, -- end_pid (required)
    ANYARRAY, -- end_pid (required)

    directed BOOLEAN DEFAULT true,
    driving_side CHAR DEFAULT 'b', -- 'r'/'l'/'b'/NULL

    OUT start_pid BIGINT,
    OUT end_pid BIGINT,
    OUT agg_cost float)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.start_pid, a.end_pid, a.agg_cost
    FROM _pgr_withPoints(_pgr_get_statement($1), $2, $3::BIGINT[], $4::BIGINT[], $5,  $6, TRUE, TRUE) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- Combinations SQL signature
--v3.2
CREATE FUNCTION pgr_withPointsCost(
    TEXT, -- edges_sql (required)
    TEXT, -- points_sql (required)
    TEXT, -- combinations_sql (required)

    directed BOOLEAN DEFAULT true,
    driving_side CHAR DEFAULT 'b', -- 'r'/'l'/'b'/NULL

    OUT start_pid BIGINT,
    OUT end_pid BIGINT,
    OUT agg_cost float)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.start_pid, a.end_pid, a.agg_cost
    FROM _pgr_withPoints(_pgr_get_statement($1), _pgr_get_statement($2), _pgr_get_statement($3), $4, $5, TRUE, TRUE) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- COMMENTS


COMMENT ON FUNCTION pgr_withPointsCost(TEXT, TEXT, BIGINT, BIGINT, BOOLEAN, CHAR)
IS 'pgr_withPointsCost (One to One)
- PROPOSED
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - Points SQL with columns: [pid], edge_id, fraction[,side]
    - From vertex/point identifier
    - To vertex/point identifier
- Optional Parameters
    - directed := ''true''
    - driving_side := ''b''
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_withPointsCost.html
';


COMMENT ON FUNCTION pgr_withPointsCost(TEXT, TEXT, BIGINT, ANYARRAY, BOOLEAN, CHAR)
IS 'pgr_withPointsCost (One to Many)
- PROPOSED
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - Points SQL with columns: [pid], edge_id, fraction[,side]
    - From vertex/point identifier
    - To ARRAY[vertices/points identifiers]
- Optional Parameters
    - directed := ''true''
    - driving_side := ''b''
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_withPointsCost.html
';


COMMENT ON FUNCTION pgr_withPointsCost(TEXT, TEXT, ANYARRAY, BIGINT, BOOLEAN, CHAR)
IS 'pgr_withPointsCost (Many to One)
- PROPOSED
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - Points SQL with columns: [pid], edge_id, fraction[,side]
    - From ARRAY[vertices/points identifiers]
    - To vertex/point identifier
- Optional Parameters
    - directed := ''true''
    - driving_side := ''b''
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_withPointsCost.html
';


COMMENT ON FUNCTION pgr_withPointsCost(TEXT, TEXT, ANYARRAY, ANYARRAY, BOOLEAN, CHAR)
IS 'pgr_withPointsCost (Many to Many)
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - Points SQL with columns: [pid], edge_id, fraction[,side]
    - From ARRAY[vertices/points identifiers]
    - To ARRAY[vertices/points identifiers]
- Optional Parameters
    - directed := ''true''
    - driving_side := ''b''
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_withPointsCost.html
';

COMMENT ON FUNCTION pgr_withPointsCost(TEXT, TEXT, TEXT, BOOLEAN, CHAR)
IS 'pgr_withPointsCost(Combinations)
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - Points SQL with columns: [pid], edge_id, fraction [,side]
    - Combinations SQL with columns: source, target
- Optional Parameters
    - directed := ''true''
    - driving_side := ''b''
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_withPointsCost.html
';


---------------------
---------------------
-- costMatrix
---------------------
---------------------

---------------------------
-- pgr_withPointsCostMatrix
---------------------------


--v2.6
CREATE FUNCTION pgr_withPointsCostMatrix(
    TEXT,     -- edges_sql (required)
    TEXT,     -- points_sql (required)
    ANYARRAY, -- pids (required)

    directed BOOLEAN DEFAULT true,
    driving_side CHAR DEFAULT 'b', -- 'r'/'l'/'b'/NULL

    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT agg_cost float)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.start_pid, a.end_pid, a.agg_cost
    FROM _pgr_withPoints(_pgr_get_statement($1), _pgr_get_statement($2), $3, $3, $4,  $5, TRUE, TRUE) AS a;
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;

-- COMMENTS

COMMENT ON FUNCTION pgr_withPointsCostMatrix(TEXT, TEXT, ANYARRAY, BOOLEAN, CHAR)
IS'pgr_withPointsCostMatrix
- PROPOSED
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - Points SQL with columns: [pid], edge_id, fraction[,side]
    - ARRAY [points identifiers],
- Optional Parameters
    - directed := true
    - driving_side := ''b''
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_withPointsCostMatrix.html
';


--v3.4
CREATE FUNCTION _pgr_withPointsVia(
  TEXT,     -- edges_sql
  TEXT,     -- points_sql
  ANYARRAY, -- via vids
  BOOLEAN,  -- directed
  BOOLEAN,  -- strict
  BOOLEAN,  -- U_turn_on_edge
  CHAR,     -- driving_side
  BOOLEAN,  -- details

  OUT seq INTEGER,
  OUT path_id INTEGER,
  OUT path_seq INTEGER,
  OUT start_vid BIGINT,
  OUT end_vid BIGINT,
  OUT node BIGINT,
  OUT edge BIGINT,
  OUT cost FLOAT,
  OUT agg_cost FLOAT,
  OUT route_agg_cost FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c VOLATILE STRICT;

COMMENT ON FUNCTION _pgr_withPointsVia(TEXT, TEXT, ANYARRAY, BOOLEAN, BOOLEAN, BOOLEAN, CHAR, BOOLEAN)
IS 'pgRouting internal function';


--v3.4
CREATE FUNCTION pgr_withPointsVia(
  TEXT,     -- edges SQL
  TEXT,     -- points SQL
  ANYARRAY, -- via vids

  directed BOOLEAN DEFAULT true,

  -- via parameters
  strict BOOLEAN DEFAULT false,
  U_turn_on_edge BOOLEAN DEFAULT true,

  -- withPoints parameters
  driving_side CHAR DEFAULT 'b', -- 'r'/'l'/'b'/NULL
  details BOOLEAN DEFAULT false,

  OUT seq INTEGER,
  OUT path_id INTEGER,
  OUT path_seq INTEGER,
  OUT start_vid BIGINT,
  OUT end_vid BIGINT,
  OUT node BIGINT,
  OUT edge BIGINT,
  OUT cost FLOAT,
  OUT agg_cost FLOAT,
  OUT route_agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
  SELECT *
  FROM _pgr_withPointsVia( _pgr_get_statement($1), _pgr_get_statement($2), $3, $4, $5, $6, $7, $8);
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;

COMMENT ON FUNCTION pgr_withPointsVia(TEXT, TEXT, ANYARRAY, BOOLEAN, BOOLEAN, BOOLEAN, CHAR, BOOLEAN)
IS 'pgr_withPointsVia
- PROPOSED
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - Points SQL with columns: [pid], edge_id, fraction [,side]
  - ARRAY[via vertices identifiers]
- Optional Parameters
  - directed := true
  - strict := false
  - U_turn_on_edge := true
  - driving_side := ''b''
  - details := ''false''
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_withPointsVia.html
';

--v2.6
CREATE FUNCTION  _pgr_withPointsVia(
    sql TEXT,
    via_edges BIGINT[],
    fraction FLOAT[],
    directed BOOLEAN DEFAULT TRUE,

    OUT seq INTEGER,
    OUT path_id INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT,
    OUT route_agg_cost FLOAT)

  RETURNS SETOF RECORD AS
  $BODY$
  DECLARE
  has_rcost boolean;
  sql_new_vertices text := ' ';
  sql_on_vertex text;
  v_union text := ' ';
  dummyrec record;
  rec1 record;
  via_vertices int[];
  sql_safe text;
  new_edges text;
  BEGIN
     BEGIN
        sql_safe = 'SELECT id, source, target, cost, reverse_cost FROM ('|| sql || ') AS __a';

        EXECUTE 'select reverse_cost, pg_typeof(reverse_cost)::text as rev_type  from ('||sql_safe||' ) AS __b__ limit 1 ' INTO rec1;
        has_rcost := true;
        EXCEPTION
          WHEN OTHERS THEN
            has_rcost = false;
     END;


      IF array_length(via_edges, 1) != array_length(fraction, 1) then
        RAISE EXCEPTION 'The length of via_edges is different of length of via_edges';
      END IF;

      FOR i IN 1 .. array_length(via_edges, 1)
      LOOP
          IF fraction[i] = 0 THEN
              sql_on_vertex := 'SELECT source FROM ('|| sql || ') __a where id = ' || via_edges[i];
              EXECUTE sql_on_vertex into dummyrec;
              via_vertices[i] = dummyrec.source;
          ELSE IF fraction[i] = 1 THEN
              sql_on_vertex := 'SELECT target FROM ('|| sql || ') __a where id = ' || via_edges[i];
              EXECUTE sql_on_vertex into dummyrec;
              via_vertices[i] = dummyrec.target;
          ELSE
              via_vertices[i] = -i;
              IF has_rcost THEN
                   sql_new_vertices = sql_new_vertices || v_union ||
                          '(SELECT id, source, ' ||  -i || ' AS target, cost * ' || fraction[i] || ' AS cost,
                              reverse_cost * (1 - ' || fraction[i] || ')  AS reverse_cost
                          FROM (SELECT * FROM (' || sql || ') __b' || i || ' WHERE id = ' || via_edges[i] || ') __a' || i ||')
                             UNION
                          (SELECT id, ' ||  -i || ' AS source, target, cost * (1 -' || fraction[i] || ') AS cost,
                              reverse_cost *  ' || fraction[i] || '  AS reverse_cost
                          FROM (SELECT * FROM (' || sql || ') __b' || i || ' where id = ' || via_edges[i] || ') __a' || i ||')';
                      v_union = ' UNION ';
               ELSE
                   sql_new_vertices = sql_new_vertices || v_union ||
                          '(SELECT id, source, ' ||  -i || ' AS target, cost * ' || fraction[i] || ' AS cost
                          FROM (SELECT * FROM (' || sql || ') __b' || i || ' WHERE id = ' || via_edges[i] || ') __a' || i ||')
                             UNION
                          (SELECT id, ' ||  -i || ' AS source, target, cost * (1 -' || fraction[i] || ') AS cost
                          FROM (SELECT * FROM (' || sql || ') __b' || i || ' WHERE id = ' || via_edges[i] || ') __a' || i ||')';
                      v_union = ' UNION ';
               END IF;
          END IF;
          END IF;
     END LOOP;

     IF sql_new_vertices = ' ' THEN
         new_edges := sql;
     ELSE
         IF has_rcost THEN
            new_edges:= 'WITH
                   orig AS ( ' || sql || '),
                   original AS (SELECT id, source, target, cost, reverse_cost FROM orig),
                   the_union AS ( ' || sql_new_vertices || '),
                   first_part AS ( SELECT * FROM (SELECT id, target AS source,  lead(target) OVER w  AS target,
                         lead(cost) OVER w  - cost AS cost,
                         lead(cost) OVER w  - cost AS reverse_cost
                      FROM  the_union  WHERE source > 0 AND cost > 0
                      WINDOW w AS (PARTITION BY id  ORDER BY cost ASC) ) as n2
                      WHERE target IS NOT NULL),
                   second_part AS ( SELECT * FROM (SELECT id, lead(source) OVER w  AS source, source as target,
                         reverse_cost - lead(reverse_cost) OVER w  AS cost,
                         reverse_cost - lead(reverse_cost) OVER w  AS reverse_cost
                      FROM  the_union  WHERE target > 0 and reverse_cost > 0
                      WINDOW w AS (PARTITION BY id  ORDER BY reverse_cost ASC) ) as n2
                      WHERE source IS NOT NULL),
                   more_union AS ( SELECT * from (
                       (SELECT * FROM original)
                             UNION
                       (SELECT * FROM the_union)
                             UNION
                       (SELECT * FROM first_part)
                             UNION
                       (SELECT * FROM second_part) ) _union )
                  SELECT *  FROM more_union';
         ELSE
            new_edges:= 'WITH
                   orig AS ( ' || sql || '),
                   original AS (SELECT id, source, target, cost FROM orig),
                   the_union AS ( ' || sql_new_vertices || '),
                   first_part AS ( SELECT * FROM (SELECT id, target AS source,  lead(target) OVER w  AS target,
                         lead(cost) OVER w  - cost AS cost
                      FROM  the_union  WHERE source > 0 AND cost > 0
                      WINDOW w AS (PARTITION BY id  ORDER BY cost ASC) ) as n2
                      WHERE target IS NOT NULL ),
                   more_union AS ( SELECT * from (
                       (SELECT * FROM original)
                             UNION
                       (SELECT * FROM the_union)
                             UNION
                       (SELECT * FROM first_part) ) _union )
                  SELECT *  FROM more_union';
          END IF;
      END IF;

     sql_new_vertices := sql_new_vertices || v_union || ' (' || sql || ')';

     RETURN query SELECT *
         FROM pgr_dijkstraVia(new_edges, via_vertices, directed, has_rcost);
  END
  $BODY$
  LANGUAGE plpgsql VOLATILE STRICT
  COST 100
  ROWS 1000;


-- COMMENTS


COMMENT ON FUNCTION _pgr_withPointsVia(TEXT, BIGINT[], FLOAT[], BOOLEAN)
IS 'pgRouting internal function DEPRECATED on v3.4.0';


----------------------
-- pgr_lineGraphFull
----------------------

--v3.0
CREATE FUNCTION _pgr_lineGraphFull(
    TEXT, -- edges_sql

    OUT seq INTEGER,
    OUT source BIGINT,
    OUT target BIGINT,
    OUT cost FLOAT,
    OUT edge BIGINT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE C IMMUTABLE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_lineGraphFull(TEXT)
IS 'pgRouting internal function';


--v2.6
CREATE FUNCTION pgr_lineGraphFull(
    TEXT, -- edges_sql (required)

    OUT seq INTEGER,
    OUT source BIGINT,
    OUT target BIGINT,
    OUT cost FLOAT,
    OUT edge BIGINT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_lineGraphFull(_pgr_get_statement($1))
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;


-- COMMENTS

COMMENT ON FUNCTION pgr_lineGraphFull(TEXT)
IS 'pgr_lineGraphFull
- EXPERIMENTAL
- For Directed Graph
- Parameters:
  - edges SQL with columns: id, source, target, cost [,reverse_cost]
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_lineGraphFull.html
';


----------------------
-- pgr_lineGraph
----------------------


--v3.0
CREATE FUNCTION _pgr_lineGraph(
    TEXT, -- edges_sql

    directed BOOLEAN,

    OUT seq INTEGER,
    OUT source BIGINT,
    OUT target BIGINT,
    OUT cost FLOAT,
    OUT reverse_cost FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c IMMUTABLE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_lineGraph(TEXT, BOOLEAN)
IS 'pgRouting internal function';


--v2.6
CREATE FUNCTION pgr_lineGraph(
    TEXT, -- edges_sql (required)

    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT source BIGINT,
    OUT target BIGINT,
    OUT cost FLOAT,
    OUT reverse_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_lineGraph(_pgr_get_statement($1), $2)
$BODY$
LANGUAGE SQL VOLATILE STRICT
COST 100
ROWS 1000;

-- COMMENTS

COMMENT ON FUNCTION pgr_lineGraph(TEXT, BOOLEAN)
IS 'pgr_lineGraph
- EXPERIMENTAL
- Parameters:
  - edges SQL with columns: id, source, target, cost [,reverse_cost]
- Optional Parameters:
  - directed := true
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_lineGraph.html
';


---------------
---------------
--  COMPONENTS
---------------
---------------

--------------------------
-- pgr_connectedComponents
--------------------------

--v3.0
CREATE FUNCTION _pgr_connectedComponents(
    edges_sql TEXT,

    OUT seq BIGINT,
    OUT component BIGINT,
    OUT node BIGINT)

RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c IMMUTABLE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_connectedComponents(TEXT)
IS 'pgRouting internal function';


---------------
---------------
--  COMPONENTS
---------------
---------------

--------------------------
-- pgr_connectedComponents
--------------------------

--v3.0
CREATE FUNCTION pgr_connectedComponents(
    TEXT, -- edges_sql (required)

    OUT seq BIGINT,
    OUT component BIGINT,
    OUT node BIGINT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_connectedComponents(_pgr_get_statement($1));
$BODY$
LANGUAGE SQL VOLATILE STRICT;


-- COMMENTS

COMMENT ON FUNCTION pgr_connectedComponents(TEXT)
IS'pgr_connectedComponents
- Undirected graph
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_connectedComponents.html
';


-----------------------
-- pgr_strongComponents
-----------------------


--v3.0
CREATE FUNCTION _pgr_strongComponents(
    edges_sql TEXT,

    OUT seq BIGINT,
    OUT component BIGINT,
    OUT node BIGINT)

RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c IMMUTABLE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_strongComponents(TEXT)
IS 'pgRouting internal function';


--v3.0
CREATE FUNCTION pgr_strongComponents(
    TEXT, -- edges_sql (required)

    OUT seq BIGINT,
    OUT component BIGINT,
    OUT node BIGINT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_strongComponents(_pgr_get_statement($1));
$BODY$
LANGUAGE SQL VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION pgr_strongComponents(TEXT)
IS'pgr_strongComponents
- Directed graph
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_strongComponents.html
';



----------------------------
-- pgr_biconnectedComponents
----------------------------


--v3.0
CREATE FUNCTION _pgr_biconnectedComponents(
    edges_sql TEXT,

    OUT seq BIGINT,
    OUT component BIGINT,
    OUT edge BIGINT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c IMMUTABLE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_biconnectedComponents(TEXT)
IS 'pgRouting internal function';


--v3.0
CREATE FUNCTION pgr_biconnectedComponents(
    TEXT, -- edges_sql (required)

    OUT seq BIGINT,
    OUT component BIGINT,
    OUT edge BIGINT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_biconnectedComponents(_pgr_get_statement($1));
$BODY$
LANGUAGE SQL VOLATILE STRICT;


-- COMMENTS

COMMENT ON FUNCTION pgr_biconnectedComponents(TEXT)
IS'pgr_biconnectedComponents
- Undirected graph
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_biconnectedComponents.html
';


-------------------------
-- pgr_articulationPoints
-------------------------

--v3.0
CREATE FUNCTION _pgr_articulationPoints(
    edges_sql TEXT,

    OUT seq INTEGER,
    OUT node BIGINT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c IMMUTABLE STRICT;


-- COMMENTS

COMMENT ON FUNCTION _pgr_articulationPoints(TEXT)
IS 'pgRouting internal function';


--v3.0
CREATE FUNCTION pgr_articulationPoints(
    TEXT,   -- edges_sql (required)

    OUT node BIGINT)
RETURNS SETOF BIGINT AS
$BODY$
    SELECT node
    FROM _pgr_articulationPoints(_pgr_get_statement($1));
$BODY$
LANGUAGE SQL VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION pgr_articulationPoints(TEXT)
IS'pgr_articulationPoints
- Undirected graph
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_articulationPoints.html
';


--------------
-- pgr_bridges
--------------

--v3.0
CREATE FUNCTION _pgr_bridges(
    edges_sql TEXT,

    OUT seq INTEGER,
    OUT edge BIGINT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE C IMMUTABLE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_bridges(TEXT)
IS 'pgRouting internal function';


--v3.0
CREATE FUNCTION pgr_bridges(
    TEXT,  -- edges_sql (required)

    OUT edge BIGINT)
RETURNS SETOF BIGINT AS
$BODY$
    SELECT edge
    FROM _pgr_bridges(_pgr_get_statement($1));
$BODY$
LANGUAGE SQL VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION pgr_bridges(TEXT)
IS'pgr_bridges
- Undirected graph
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_bridges.html
';

-------------------------
-------------------------
-- _makeConnected
-------------------------
-------------------------

--v3.2
CREATE FUNCTION _pgr_makeConnected(
  TEXT,   -- edges_sql (required)

  OUT seq BIGINT,
  OUT start_vid BIGINT,
  OUT end_vid BIGINT)

RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c IMMUTABLE STRICT;


-- COMMENTS

COMMENT ON FUNCTION _pgr_makeConnected(TEXT)
IS 'pgRouting internal function';


------------------
-- pgr_makeConnected
------------------

--v3.2
CREATE FUNCTION pgr_makeConnected(
    TEXT,   -- edges_sql (required)
    OUT seq BIGINT,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT)

RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_makeConnected(_pgr_get_statement($1)) AS a;
$BODY$
LANGUAGE SQL VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION pgr_makeConnected(TEXT)
IS 'pgr_makeConnected
- EXPERIMENTAL
- Undirected graph
- Parameters:
  - edges SQL with columns: id, source, target, cost [,reverse_cost]
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_makeConnected.html
';

-------------------------
-------------------------
-- bellman_ford
-------------------------
-------------------------

--v3.0
CREATE FUNCTION _pgr_bellmanFord(
    edges_sql TEXT,
    from_vids ANYARRAY,
    to_vids   ANYARRAY,
    directed  BOOLEAN,
    only_cost BOOLEAN,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c IMMUTABLE STRICT;


--v3.2
CREATE FUNCTION _pgr_bellmanFord(
    edges_sql TEXT,
    combinations_sql TEXT,
    directed  BOOLEAN,
    only_cost BOOLEAN,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE C IMMUTABLE STRICT;


-- COMMENTS

COMMENT ON FUNCTION _pgr_bellmanFord(TEXT, ANYARRAY, ANYARRAY, BOOLEAN, BOOLEAN)
IS 'pgRouting internal function';

COMMENT ON FUNCTION _pgr_bellmanFord(TEXT, TEXT, BOOLEAN, BOOLEAN)
IS 'pgRouting internal function';


------------------
-- pgr_bellmanFord
------------------


--ONE TO ONE
--v3.0
CREATE FUNCTION pgr_bellmanFord(
    TEXT,   -- edges_sql (required)
    BIGINT, -- from_vid (required)
    BIGINT, -- to_vid (required)

    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_bellmanFord(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], ARRAY[$3]::BIGINT[], directed, false) AS a;
$BODY$
LANGUAGE SQL VOLATILE STRICT;


--ONE TO MANY
--v3.0
CREATE FUNCTION pgr_bellmanFord(
    TEXT,     -- edges_sql (required)
    BIGINT,   -- from_vid (required)
    ANYARRAY, -- to_vids (required)

    directed BOOLEAN DEFAULT true,


    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.end_vid, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_bellmanFord(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], $3::BIGINT[], directed, false) AS a;
$BODY$
LANGUAGE SQL VOLATILE STRICT;


--MANY TO ONE
--v3.0
CREATE FUNCTION pgr_bellmanFord(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    BIGINT,   -- to_vid (required)

    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.start_vid, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_bellmanFord(_pgr_get_statement($1), $2::BIGINT[], ARRAY[$3]::BIGINT[], directed, false) AS a;
$BODY$
LANGUAGE SQL VOLATILE STRICT;


--MANY TO MANY
--v3.0
CREATE FUNCTION pgr_bellmanFord(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    ANYARRAY, -- to_vids (required)

    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.start_vid, a.end_vid, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_bellmanFord(_pgr_get_statement($1), $2::BIGINT[], $3::BIGINT[], directed, false ) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT;


--COMBINATIONS
--v3.2
CREATE FUNCTION pgr_bellmanFord(
    TEXT,     -- edges_sql (required)
    TEXT,     -- combinations_sql (required)

    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.start_vid, a.end_vid, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_bellmanFord(_pgr_get_statement($1), _pgr_get_statement($2), directed, false ) AS a;
$BODY$
LANGUAGE SQL VOLATILE STRICT;


-- COMMENTS


COMMENT ON FUNCTION pgr_bellmanFord(TEXT, BIGINT, BIGINT, BOOLEAN)
IS 'pgr_bellmanFord(One to One)
- EXPERIMENTAL
- Parameters:
  - edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From vertex identifier
  - To vertex identifier
- Optional Parameters:
  - directed := true
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_bellmanFord.html
';


COMMENT ON FUNCTION pgr_bellmanFord(TEXT, BIGINT, ANYARRAY, BOOLEAN)
IS 'pgr_bellmanFord(One to Many)
- EXPERIMENTAL
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From vertex identifier
  - To ARRAY[vertices identifiers]
- Optional Parameters
  - directed := true
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_bellmanFord.html
';


COMMENT ON FUNCTION pgr_bellmanFord(TEXT, ANYARRAY, BIGINT, BOOLEAN)
IS 'pgr_bellmanFord(Many to One)
- EXPERIMENTAL
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From ARRAY[vertices identifiers]
  - To vertex identifier
- Optional Parameters
  - directed := true
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_bellmanFord.html
';


COMMENT ON FUNCTION pgr_bellmanFord(TEXT, ANYARRAY, ANYARRAY, BOOLEAN)
IS 'pgr_bellmanFord(Many to Many)
- EXPERIMENTAL
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From ARRAY[vertices identifiers]
  - To ARRAY[vertices identifiers]
- Optional Parameters
  - directed := true
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_bellmanFord.html
';


COMMENT ON FUNCTION pgr_bellmanFord(TEXT, TEXT, BOOLEAN)
IS 'pgr_bellmanFord(Combinations)
- EXPERIMENTAL
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - Combinations SQL with columns: source, target
- Optional Parameters
  - directed := true
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_bellmanFord.html
';

-------------------------
-------------------------
-- _pgr_edwardMoore
-------------------------
-------------------------

--v3.0
CREATE FUNCTION _pgr_edwardMoore(
    edges_sql TEXT,
    from_vids ANYARRAY,
    to_vids   ANYARRAY,
    directed  BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c IMMUTABLE STRICT;


--v3.2
CREATE FUNCTION _pgr_edwardMoore(
    edges_sql TEXT,
    combinations_sql TEXT,
    directed  BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE C IMMUTABLE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_edwardMoore(TEXT, ANYARRAY, ANYARRAY, BOOLEAN)
IS 'pgRouting internal function';

COMMENT ON FUNCTION _pgr_edwardMoore(TEXT, TEXT, BOOLEAN)
IS 'pgRouting internal function';

---------------
-- pgr_edwardMoore
---------------

-- ONE to ONE
--v3.0
CREATE FUNCTION pgr_edwardMoore(
    TEXT,   -- edges_sql (required)
    BIGINT, -- from_vid (required)
    BIGINT, -- to_vid (required)

    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_edwardMoore(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], ARRAY[$3]::BIGINT[], $4) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT;


-- ONE to MANY
--v3.0
CREATE FUNCTION pgr_edwardMoore(
    TEXT,     -- edges_sql (required)
    BIGINT,   -- from_vid (required)
    ANYARRAY, -- to_vids (required)

    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.end_vid, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_edwardMoore(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], $3::BIGINT[], $4) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT;


-- MANY to ONE
--v3.0
CREATE FUNCTION pgr_edwardMoore(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    BIGINT,   -- to_vid (required)

    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.start_vid, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_edwardMoore(_pgr_get_statement($1), $2::BIGINT[], ARRAY[$3]::BIGINT[], $4) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT;


-- MANY to MANY
--v3.0
CREATE FUNCTION pgr_edwardMoore(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    ANYARRAY, -- to_vids (required)

    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.start_vid, a.end_vid, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_edwardMoore(_pgr_get_statement($1), $2::BIGINT[], $3::BIGINT[], $4) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT;


-- COMBINATIONS
--v3.2
CREATE FUNCTION pgr_edwardMoore(
    TEXT,     -- edges_sql (required)
    TEXT,     -- combinations_sql (required)

    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.start_vid, a.end_vid, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_edwardMoore(_pgr_get_statement($1), _pgr_get_statement($2), $3) AS a;
$BODY$
LANGUAGE SQL VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION pgr_edwardMoore(TEXT, BIGINT, BIGINT, BOOLEAN)
IS 'pgr_edwardMoore(One to One)
- EXPERIMENTAL
- Parameters:
   - Edges SQL with columns: id, source, target, cost [,reverse_cost]
   - From vertex identifier
   - To vertex identifier
- Optional Parameters
   - directed := true
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_edwardMoore.html
';

COMMENT ON FUNCTION pgr_edwardMoore(TEXT, BIGINT, ANYARRAY, BOOLEAN)
IS 'pgr_edwardMoore(One to Many)
- EXPERIMENTAL
- Parameters:
   - Edges SQL with columns: id, source, target, cost [,reverse_cost]
   - From vertex identifier
   - To ARRAY[vertices identifiers]
- Optional Parameters
   - directed := true
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_edwardMoore.html
';

COMMENT ON FUNCTION pgr_edwardMoore(TEXT, ANYARRAY, BIGINT, BOOLEAN)
IS 'pgr_edwardMoore(Many to One)
- EXPERIMENTAL
- Parameters:
   - Edges SQL with columns: id, source, target, cost [,reverse_cost]
   - From ARRAY[vertices identifiers]
   - To vertex identifier
- Optional Parameters
   - directed := true
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_edwardMoore.html
';

COMMENT ON FUNCTION pgr_edwardMoore(TEXT, ANYARRAY, ANYARRAY, BOOLEAN)
IS 'pgr_edwardMoore(Many to Many)
- EXPERIMENTAL
- Parameters:
   - Edges SQL with columns: id, source, target, cost [,reverse_cost]
   - From ARRAY[vertices identifiers]
   - To ARRAY[vertices identifiers]
- Optional Parameters
   - directed := true
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_edwardMoore.html
';

COMMENT ON FUNCTION pgr_edwardMoore(TEXT, TEXT, BOOLEAN)
IS 'pgr_edwardMoore(Combinations)
- EXPERIMENTAL
- Parameters:
   - Edges SQL with columns: id, source, target, cost [,reverse_cost]
   - Combinations SQL with columns: source, target
- Optional Parameters
   - directed := true
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_edwardMoore.html
';


---------------------
---------------------
-- dagShortestPath
---------------------
---------------------

---------------------------
--_pgr_dagShortestPath
---------------------------

--v3.0
CREATE FUNCTION _pgr_dagShortestPath(
    TEXT,
    ANYARRAY,
    ANYARRAY,
    directed BOOLEAN DEFAULT true,
    only_cost BOOLEAN DEFAULT false,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c IMMUTABLE STRICT;

--v3.2
CREATE FUNCTION _pgr_dagShortestPath(
    TEXT,
    TEXT,
    directed BOOLEAN DEFAULT true,
    only_cost BOOLEAN DEFAULT false,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE C IMMUTABLE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_dagShortestPath(TEXT, ANYARRAY, ANYARRAY, BOOLEAN, BOOLEAN)
IS 'pgRouting internal function';

COMMENT ON FUNCTION _pgr_dagShortestPath(TEXT, TEXT, BOOLEAN, BOOLEAN)
IS 'pgRouting internal function';


----------------------
-- pgr_dagShortestPath
----------------------

-- ONE to ONE
--v3.0
CREATE FUNCTION pgr_dagShortestPath(
    TEXT,     -- edges_sql (required)
    BIGINT,   -- from_vid (required)
    BIGINT,   -- from_vid (required)

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_dagShortestPath(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], ARRAY[$3]::BIGINT[], true, false ) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- ONE to MANY
--v3.0
CREATE FUNCTION pgr_dagShortestPath(
    TEXT,     -- edges_sql (required)
    BIGINT,   -- from_vid (required)
    ANYARRAY, -- to_vids (required)

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_dagShortestPath(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], $3::BIGINT[], true, false ) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- MANY to ONE
--v3.0
CREATE FUNCTION pgr_dagShortestPath(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    BIGINT,   -- to_vid (required)

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_dagShortestPath(_pgr_get_statement($1), $2::BIGINT[], ARRAY[$3]::BIGINT[], true, false ) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- MANY to MANY
--v3.0
CREATE FUNCTION pgr_dagShortestPath(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    ANYARRAY, -- to_vids (required)

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_dagShortestPath(_pgr_get_statement($1), $2::BIGINT[], $3::BIGINT[], true, false ) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;


-- COMBINATIONS
--v3.2
CREATE FUNCTION pgr_dagShortestPath(
    TEXT,     -- edges_sql (required)
    TEXT,     -- combinations_sql (required)

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_dagShortestPath(_pgr_get_statement($1), _pgr_get_statement($2), true, false ) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT
COST 100
ROWS 1000;

-- COMMENTS

COMMENT ON FUNCTION pgr_dagShortestPath(TEXT, BIGINT, BIGINT)
IS 'pgr_dagShortestPath(One to One)
- EXPERIMENTAL
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - From vertex identifier
    - To vertex identifier
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_dagShortestPath.html
';

COMMENT ON FUNCTION pgr_dagShortestPath(TEXT, BIGINT, ANYARRAY)
IS 'pgr_dagShortestPath(One to Many)
- EXPERIMENTAL
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From vertex identifier
  - To ARRAY[vertices identifiers]
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_dagShortestPath.html
';

COMMENT ON FUNCTION pgr_dagShortestPath(TEXT, ANYARRAY, BIGINT)
IS 'pgr_dagShortestPath(Many to One)
- EXPERIMENTAL
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From ARRAY[vertices identifiers]
  - To vertex identifier
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_dagShortestPath.html
';

COMMENT ON FUNCTION pgr_dagShortestPath(TEXT, ANYARRAY, ANYARRAY)
IS 'pgr_dagShortestPath(Many to Many)
- EXPERIMENTAL
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From ARRAY[vertices identifiers]
  - To ARRAY[vertices identifiers]
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_dagShortestPath.html
';

COMMENT ON FUNCTION pgr_dagShortestPath(TEXT, TEXT)
IS 'pgr_dagShortestPath(Combinations)
- EXPERIMENTAL
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - Combinations SQL with columns: source, target
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_dagShortestPath.html
';

----------
----------
-- ChPP
----------
----------

--v3.0
CREATE FUNCTION _pgr_chinesePostman(
    edges_sql TEXT,

    only_cost BOOLEAN,

    OUT seq INTEGER,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_chinesePostman(TEXT, BOOLEAN)
IS 'pgRouting internal function';



--------------------
-- pgr_directedChPP
--------------------



--v3.0
CREATE FUNCTION pgr_chinesePostman(
    TEXT, -- edges_sql (required)

    OUT seq INTEGER,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_chinesePostman(_pgr_get_statement($1), only_cost := false);
$BODY$
LANGUAGE SQL VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION pgr_chinesePostman(TEXT)
IS 'pgr_chinesePostman
- EXPERIMENTAL
- Directed graph
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_chinesePostman.html
';



--------------------
-- pgr_chinesePostmanCost
--------------------



--v3.0
CREATE FUNCTION pgr_chinesePostmanCost(
    TEXT -- edges_sql (required)
)
RETURNS FLOAT AS
$BODY$
    SELECT cost
    FROM _pgr_chinesePostman(_pgr_get_statement($1), only_cost := true);
$BODY$
LANGUAGE SQL VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION pgr_chinesePostmanCost(TEXT)
IS 'pgr_chinesePostmanCost
- EXPERIMENTAL
- Directed graph
- Parameters:
	- Edges SQL with columns: id, source, target, cost [,reverse_cost]
- Documentation:
	- https://docs.pgrouting.org/latest/en/pgr_chinesePostmanCost.html
';


----------
----------
-- mst
----------
----------


----------------
-- _pgr_prim
----------------


--v3.0
CREATE FUNCTION _pgr_prim(
    TEXT,             -- Edge sql
    ANYARRAY,         -- tree root for traversal
    order_by TEXT,
    max_depth BIGINT,
    distance FLOAT,

    OUT seq BIGINT,
    OUT depth BIGINT,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE C VOLATILE STRICT;


-- COMMENTS


COMMENT ON FUNCTION _pgr_prim(TEXT, ANYARRAY, TEXT, BIGINT, FLOAT)
IS 'pgRouting internal function';



----------------
-- _pgr_kruskal
----------------


--v3.0
CREATE FUNCTION _pgr_kruskal(
    TEXT,             -- Edge sql
    ANYARRAY,         -- tree root for traversal
    fn_suffix TEXT,
    max_depth BIGINT,
    distance FLOAT,

    OUT seq BIGINT,
    OUT depth BIGINT,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE C VOLATILE STRICT;


-- COMMENTS


COMMENT ON FUNCTION _pgr_kruskal(TEXT, ANYARRAY, TEXT, BIGINT, FLOAT)
IS 'pgRouting internal function';



------------
-- pgr_prim
------------


--v3.0
CREATE FUNCTION pgr_prim(
    TEXT,  -- edges_sql (required)

    OUT edge BIGINT,
    OUT cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT edge, cost
    FROM _pgr_prim(_pgr_get_statement($1), ARRAY[0]::BIGINT[], '', -1, -1);
$BODY$
LANGUAGE sql VOLATILE STRICT;


-- COMMENT


COMMENT ON FUNCTION pgr_prim(TEXT)
IS 'pgr_prim
- Undirected graph
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_prim.html
';


-----------------
-- pgr_primDFS
-----------------


-- SINGLE VERTEX
--v3.0
CREATE FUNCTION pgr_primDFS(
    TEXT,   -- Edge sql
    BIGINT, -- root vertex

    max_depth BIGINT DEFAULT 9223372036854775807,

    OUT seq BIGINT,
    OUT depth BIGINT,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
BEGIN
    IF $3 < 0 THEN
        RAISE EXCEPTION 'Negative value found on ''max_depth'''
        USING HINT = format('Value found: %s', $3);
    END IF;


    RETURN QUERY
    SELECT *
    FROM _pgr_prim(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], 'DFS', $3, -1);
END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;


-- MULTIPLE VERTICES
--v3.0
CREATE FUNCTION pgr_primDFS(
    TEXT,     -- Edge sql
    ANYARRAY, -- root vertices

    max_depth BIGINT DEFAULT 9223372036854775807,

    OUT seq BIGINT,
    OUT depth BIGINT,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
BEGIN
    IF $3 < 0 THEN
        RAISE EXCEPTION 'Negative value found on ''max_depth'''
        USING HINT = format('Value found: %s', $3);
    END IF;


    RETURN QUERY
    SELECT *
    FROM _pgr_prim(_pgr_get_statement($1), $2, 'DFS', $3, -1);
END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;


-- COMMENTS


COMMENT ON FUNCTION pgr_primDFS(TEXT, BIGINT, BIGINT)
IS 'pgr_primDFS(Single Vertex)
- Undirected graph
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - From root vertex identifier
- Optional parameters
    - max_depth := 9223372036854775807
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_primDFS.html
';

COMMENT ON FUNCTION pgr_primDFS(TEXT, ANYARRAY, BIGINT)
IS 'pgr_primDFS(Multiple Vertices)
- Undirected graph
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - From ARRAY[root vertices identifiers]
- Optional parameters
    - max_depth := 9223372036854775807
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_primDFS.html
';



-----------------
-- pgr_primBFS
-----------------


-- SINGLE VERTEX
--v3.0
CREATE FUNCTION pgr_primBFS(
    TEXT,   -- Edge sql
    BIGINT, -- root vertex

    max_depth BIGINT DEFAULT 9223372036854775807,

    OUT seq BIGINT,
    OUT depth BIGINT,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
BEGIN
    IF $3 < 0 THEN
        RAISE EXCEPTION 'Negative value found on ''max_depth'''
        USING HINT = format('Value found: %s', $3);
    END IF;


    RETURN QUERY
    SELECT *
    FROM _pgr_prim(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], 'BFS', $3, -1);
END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;


-- MULTIPLE VERTICES
--v3.0
CREATE FUNCTION pgr_primBFS(
    TEXT,     -- Edge sql
    ANYARRAY, -- root vertices

    max_depth BIGINT DEFAULT 9223372036854775807,

    OUT seq BIGINT,
    OUT depth BIGINT,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
BEGIN
    IF $3 < 0 THEN
        RAISE EXCEPTION 'Negative value found on ''max_depth'''
        USING HINT = format('Value found: %s', $3);
    END IF;


    RETURN QUERY
    SELECT *
    FROM _pgr_prim(_pgr_get_statement($1), $2, 'BFS', $3, -1);
END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;


-- COMMENTS


COMMENT ON FUNCTION pgr_primBFS(TEXT, BIGINT, BIGINT)
IS 'pgr_primBFS(Single Vertex)
- Undirected graph
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - From root vertex identifier
- Optional parameters
    - max_depth := 9223372036854775807
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_primBFS.html
';


COMMENT ON FUNCTION pgr_primBFS(TEXT, ANYARRAY, BIGINT)
IS 'pgr_primBFS(multiple Vertices)
- Undirected graph
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - From ARRAY[root vertices identifiers]
- Optional parameters
    - max_depth := 9223372036854775807
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_primBFS.html
';


--------------
-- pgr_primDD
--------------


-- SINGLE VERTEX
--v3.0
CREATE FUNCTION pgr_primDD (
    TEXT,   -- Edge sql
    BIGINT, -- root vertex
    NUMERIC,  -- distance

    OUT seq BIGINT,
    OUT depth BIGINT,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
BEGIN
    IF $3 < 0 THEN
        RAISE EXCEPTION 'Negative value found on ''distance'''
        USING HINT = format('Value found: %s', $3);
    END IF;

    RETURN QUERY
    SELECT *
    FROM _pgr_prim(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], 'DD', -1, $3::FLOAT);
END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;

--v3.0
CREATE FUNCTION pgr_primDD (
    TEXT,   -- Edge sql
    BIGINT, -- root vertex
    FLOAT,  -- distance

    OUT seq BIGINT,
    OUT depth BIGINT,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
BEGIN
    IF $3 < 0 THEN
        RAISE EXCEPTION 'Negative value found on ''distance'''
        USING HINT = format('Value found: %s', $3);
    END IF;

    RETURN QUERY
    SELECT *
    FROM _pgr_prim(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], 'DD', -1, $3::FLOAT);
END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;


-- MULTIPLE VERTICES
--v3.0
CREATE FUNCTION pgr_primDD (
    TEXT,   -- Edge sql
    ANYARRAY, -- root vertex

    NUMERIC, -- distance

    OUT seq BIGINT,
    OUT depth BIGINT,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
BEGIN
    IF $3 < 0 THEN
        RAISE EXCEPTION 'Negative value found on ''distance'''
        USING HINT = format('Value found: %s', $3);
    END IF;

    RETURN QUERY
    SELECT *
    FROM _pgr_prim(_pgr_get_statement($1), $2, 'DD', -1, $3);
END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;


--v3.0
CREATE FUNCTION pgr_primDD (
    TEXT,   -- Edge sql
    ANYARRAY, -- root vertex

    FLOAT, -- distance

    OUT seq BIGINT,
    OUT depth BIGINT,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
BEGIN
    IF $3 < 0 THEN
        RAISE EXCEPTION 'Negative value found on ''distance'''
        USING HINT = format('Value found: %s', $3);
    END IF;

    RETURN QUERY
    SELECT *
    FROM _pgr_prim(_pgr_get_statement($1), $2, 'DD', -1, $3::FLOAT);
END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;


-- COMMENTS


COMMENT ON FUNCTION pgr_primDD(TEXT, BIGINT, NUMERIC)
IS 'pgr_primDD(Single Vertex)
- Undirected graph
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - From root vertex identifier
    - Distance
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_primDD.html
';


COMMENT ON FUNCTION pgr_primDD(TEXT, ANYARRAY, NUMERIC)
IS 'pgr_primDD(Multiple Vertices)
- Undirected graph
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - From ARRAY[root vertices identifiers]
    - Distance
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_primDD.html
';


COMMENT ON FUNCTION pgr_primDD(TEXT, BIGINT, FLOAT)
IS 'pgr_primDD(Single Vertex)
- Undirected graph
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - From root vertex identifier
    - Distance
- DocumentatiEdgeson:
    - https://docs.pgrouting.org/latest/en/pgr_primDD.html
';


COMMENT ON FUNCTION pgr_primDD(TEXT, ANYARRAY, FLOAT)
IS 'pgr_primDD(Multiple Vertices)
- Undirected graph
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - From ARRAY[root vertices identifiers]
    - Distance
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_primDD.html
';



---------------
-- pgr_kruskal
---------------


--v3.0
CREATE FUNCTION pgr_kruskal(
    TEXT, -- edges-sql (required)

    OUT edge BIGINT,
    OUT cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT edge, cost
    FROM _pgr_kruskal(_pgr_get_statement($1), ARRAY[0]::BIGINT[], '', -1, -1);
$BODY$
LANGUAGE SQL VOLATILE STRICT;


-- COMMENTS


COMMENT ON FUNCTION pgr_kruskal(TEXT)
IS 'pgr_kruskal
- Undirected graph
- Parameters:
	- Edges SQL with columns: id, source, target, cost [,reverse_cost]
- Documentation:
	- https://docs.pgrouting.org/latest/en/pgr_kruskal.html
';



-----------------
-- pgr_kruskalDFS
-----------------


-- SINGLE VERTEX
--v3.0
CREATE FUNCTION pgr_kruskalDFS(
    TEXT,   -- Edge sql
    BIGINT, -- root vertex

    max_depth BIGINT DEFAULT 9223372036854775807,

    OUT seq BIGINT,
    OUT depth BIGINT,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
BEGIN
    IF $3 < 0 THEN
        RAISE EXCEPTION 'Negative value found on ''max_depth'''
        USING HINT = format('Value found: %s', $3);
    END IF;


    RETURN QUERY
    SELECT *
    FROM _pgr_kruskal(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], 'DFS', $3, -1);
END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;

-- MULTIPLE VERTICES
--v3.0
CREATE FUNCTION pgr_kruskalDFS(
    TEXT,     -- Edge sql
    ANYARRAY, -- root vertices

    max_depth BIGINT DEFAULT 9223372036854775807,

    OUT seq BIGINT,
    OUT depth BIGINT,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
BEGIN
    IF $3 < 0 THEN
        RAISE EXCEPTION 'Negative value found on ''max_depth'''
        USING HINT = format('Value found: %s', $3);
    END IF;


    RETURN QUERY
    SELECT *
    FROM _pgr_kruskal(_pgr_get_statement($1), $2, 'DFS', $3, -1);
END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;


-- COMMENTS


COMMENT ON FUNCTION pgr_kruskalDFS(TEXT, BIGINT, BIGINT)
IS 'pgr_kruskalDFS(Single Vertex)
- Undirected graph
- Parameters:
  - edges SQL with columns: id, source, target, cost [,reverse_cost]
  - from root vertex identifier
- Optional parameters
  - max_depth: default 9223372036854775807
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_kruskalDFS.html
';


COMMENT ON FUNCTION pgr_kruskalDFS(TEXT, ANYARRAY, BIGINT)
IS 'pgr_kruskalDFS(Multiple Vertices)
- Undirected graph
- Parameters:
  - edges SQL with columns: id, source, target, cost [,reverse_cost]
  - from ARRAY[root vertices identifiers]
- Optional parameters
  - max_depth: default 9223372036854775807
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_kruskalDFS.html
';



-----------------
-- pgr_kruskalBFS
-----------------


--v3.0
CREATE FUNCTION pgr_kruskalBFS(
    TEXT,   -- Edge sql (required)
    BIGINT, -- root vertex (required)

    max_depth BIGINT DEFAULT 9223372036854775807,

    OUT seq BIGINT,
    OUT depth BIGINT,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
BEGIN
    IF $3 < 0 THEN
        RAISE EXCEPTION 'Negative value found on ''max_depth'''
        USING HINT = format('Value found: %s', $3);
    END IF;


    RETURN QUERY
    SELECT *
    FROM _pgr_kruskal(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], 'BFS', $3, -1);
END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;


--v3.0
CREATE FUNCTION pgr_kruskalBFS(
    TEXT,     -- Edge sql (required)
    ANYARRAY, -- root vertices (required)

    max_depth BIGINT DEFAULT 9223372036854775807,

    OUT seq BIGINT,
    OUT depth BIGINT,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
BEGIN
    IF $3 < 0 THEN
        RAISE EXCEPTION 'Negative value found on ''max_depth'''
        USING HINT = format('Value found: %s', $3);
    END IF;


    RETURN QUERY
    SELECT *
    FROM _pgr_kruskal(_pgr_get_statement($1), $2, 'BFS', $3, -1);
END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;


-- COMMENTS


COMMENT ON FUNCTION pgr_kruskalBFS(TEXT, BIGINT, BIGINT)
IS 'pgr_kruskalBFS(Single Vertex)
- Undirected graph
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - From root vertex identifier
- Optional parameters
    - max_depth: default := 9223372036854775807
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_kruskalBFS.html
';


COMMENT ON FUNCTION pgr_kruskalBFS(TEXT, ANYARRAY, BIGINT)
IS 'pgr_kruskalBFS(multiple Vertices)
- Undirected graph
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - From ARRAY[root vertices identifiers]
- Optional parameters
    - max_depth: default := 9223372036854775807
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_kruskalBFS.html
';



-----------------
-- pgr_kruskalDD
-----------------


-- SINGLE VERTEX
--v3.0
CREATE FUNCTION pgr_kruskalDD (
    TEXT,   -- Edge sql
    BIGINT, -- root vertex
    NUMERIC,  -- distance

    OUT seq BIGINT,
    OUT depth BIGINT,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
BEGIN
    IF $3 < 0 THEN
        RAISE EXCEPTION 'Negative value found on ''distance'''
        USING HINT = format('Value found: %s', $3);
    END IF;

    RETURN QUERY
    SELECT *
    FROM _pgr_kruskal(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], 'DD', -1, $3::FLOAT);
END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;

--v3.0
CREATE FUNCTION pgr_kruskalDD (
    TEXT,   -- Edge sql
    BIGINT, -- root vertex
    FLOAT,  -- distance

    OUT seq BIGINT,
    OUT depth BIGINT,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
BEGIN
    IF $3 < 0 THEN
        RAISE EXCEPTION 'Negative value found on ''distance'''
        USING HINT = format('Value found: %s', $3);
    END IF;

    RETURN QUERY
    SELECT *
    FROM _pgr_kruskal(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], 'DD', -1, $3::FLOAT);
END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;


-- MULTIPLE VERTICES
--v3.0
CREATE FUNCTION pgr_kruskalDD (
    TEXT,   -- Edge sql
    ANYARRAY, -- root vertex

    NUMERIC, -- distance

    OUT seq BIGINT,
    OUT depth BIGINT,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
BEGIN
    IF $3 < 0 THEN
        RAISE EXCEPTION 'Negative value found on ''distance'''
        USING HINT = format('Value found: %s', $3);
    END IF;

    RETURN QUERY
    SELECT *
    FROM _pgr_kruskal(_pgr_get_statement($1), $2, 'DD', -1, $3);
END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;


--v3.0
CREATE FUNCTION pgr_kruskalDD (
    TEXT,   -- Edge sql
    ANYARRAY, -- root vertex

    FLOAT, -- distance

    OUT seq BIGINT,
    OUT depth BIGINT,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
BEGIN
    IF $3 < 0 THEN
        RAISE EXCEPTION 'Negative value found on ''distance'''
        USING HINT = format('Value found: %s', $3);
    END IF;

    RETURN QUERY
    SELECT *
    FROM _pgr_kruskal(_pgr_get_statement($1), $2, 'DD', -1, $3::FLOAT);
END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;


-- COMMENTS


COMMENT ON FUNCTION pgr_kruskalDD(TEXT, BIGINT, NUMERIC)
IS 'pgr_kruskalDD(Single Vertex)
- Undirected graph
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - From root vertex identifier
    - Distance
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_kruskalDD.html
';


COMMENT ON FUNCTION pgr_kruskalDD(TEXT, ANYARRAY, NUMERIC)
IS 'pgr_kruskalDD(Multiple Vertices)
- Undirected graph
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - From ARRAY[root vertices identifiers]
    - Distance
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_kruskalDD.html
';


COMMENT ON FUNCTION pgr_kruskalDD(TEXT, BIGINT, FLOAT)
IS 'pgr_kruskalDD(Single Vertex)
- Undirected graph
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - From root vertex identifier
    - Distance
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_kruskalDD.html
';


COMMENT ON FUNCTION pgr_kruskalDD(TEXT, ANYARRAY, FLOAT)
IS 'pgr_kruskalDD(Multiple Vertices)
- Undirected graph
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - From ARRAY[root vertices identifiers]
    - Distance
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_kruskalDD.html
';


---------------
---------------
-- mincut
---------------
---------------

-------------------
-- pgr_stoerWagner
-------------------

--v3.0
CREATE FUNCTION _pgr_stoerWagner(
    edges_sql TEXT,

    OUT seq INTEGER,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT mincut FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_stoerWagner(TEXT)
IS 'pgRouting internal function';


--v3.0
CREATE FUNCTION pgr_stoerWagner(
    TEXT, -- edges_sql (required)

    OUT seq INTEGER,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT mincut FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_stoerWagner(_pgr_get_statement($1));
$BODY$
LANGUAGE SQL VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION pgr_stoerWagner(TEXT)
IS 'pgr_stoerWagner
- EXPERIMENTAL
- Undirected graph
- Parameters:
  - edges SQL with columns: id, source, target, cost [,reverse_cost]
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_stoerWagner.html
';


--v3.0
CREATE FUNCTION _pgr_boost_version()
RETURNS TEXT AS
'MODULE_PATHNAME'
LANGUAGE C VOLATILE STRICT;

COMMENT ON FUNCTION _pgr_boost_version() IS
'pgRouting internal function';

---

--v3.0
CREATE FUNCTION _pgr_build_type()
RETURNS TEXT AS
'MODULE_PATHNAME'
LANGUAGE C VOLATILE STRICT;

COMMENT ON FUNCTION _pgr_build_type() IS
'pgRouting internal function';

---

--v3.0
CREATE FUNCTION _pgr_compilation_date()
RETURNS TEXT AS
'MODULE_PATHNAME'
LANGUAGE C VOLATILE STRICT;

COMMENT ON FUNCTION _pgr_compilation_date() IS
'pgRouting internal function';

---

--v3.0
CREATE FUNCTION _pgr_compiler_version()
RETURNS TEXT AS
'MODULE_PATHNAME'
LANGUAGE C VOLATILE STRICT;

COMMENT ON FUNCTION _pgr_compiler_version() IS
'pgRouting internal function';

---

--v3.0
CREATE FUNCTION _pgr_git_hash()
RETURNS TEXT AS
'MODULE_PATHNAME'
LANGUAGE C VOLATILE STRICT;

COMMENT ON FUNCTION _pgr_git_hash() IS
'pgRouting internal function';

---

--v3.0
CREATE FUNCTION _pgr_lib_version()
RETURNS TEXT AS
'MODULE_PATHNAME'
LANGUAGE C VOLATILE STRICT;

COMMENT ON FUNCTION _pgr_lib_version() IS
'pgRouting internal function';


---

--v3.0
CREATE FUNCTION _pgr_operating_system()
RETURNS TEXT AS
'MODULE_PATHNAME'
LANGUAGE C VOLATILE STRICT;

COMMENT ON FUNCTION _pgr_operating_system() IS
'pgRouting internal function';
---

--v3.0
CREATE FUNCTION _pgr_pgsql_version()
RETURNS TEXT AS
'MODULE_PATHNAME'
LANGUAGE C VOLATILE STRICT;

COMMENT ON FUNCTION _pgr_pgsql_version() IS
'pgRouting internal function';



--v3.0
CREATE FUNCTION pgr_version()
RETURNS TEXT AS
$BODY$
    SELECT '3.5.0'::varchar AS pgr_version;
$BODY$
LANGUAGE sql IMMUTABLE;

COMMENT ON FUNCTION pgr_version() IS
'pgr_version
- Documentation
  - https://docs.pgrouting.org/latest/en/pgr_version.html
';




--v3.0
CREATE FUNCTION pgr_full_version(
    OUT version TEXT,
    OUT build_type TEXT,
    OUT compile_date TEXT,
    OUT library TEXT,
    OUT system TEXT,
    OUT PostgreSQL TEXT,
    OUT compiler TEXT,
    OUT boost TEXT,
    OUT hash TEXT
)
RETURNS Record AS
$BODY$
    SELECT pgr_version(),
        _pgr_build_type(),
        _pgr_compilation_date(),
        _pgr_lib_version(),
        _pgr_operating_system(),
        _pgr_pgsql_version(),
        _pgr_compiler_version(),
        _pgr_boost_version(),
        _pgr_git_hash()
$BODY$
LANGUAGE sql IMMUTABLE;

COMMENT ON FUNCTION pgr_full_version() IS
'pgr_full_version
- Documentation
  - https://docs.pgrouting.org/latest/en/pgr_full_version.html
';


---------------
-- pgr_topologicalSort
---------------


--v3.0
CREATE FUNCTION _pgr_topologicalSort(
    edges_sql TEXT,

    OUT seq INTEGER,
    OUT sorted_v BIGINT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c VOLATILE STRICT;

-- COMMENTS


COMMENT ON FUNCTION _pgr_topologicalSort(TEXT)
IS 'pgRouting internal function';



---------------
-- pgr_topologicalSort
---------------


--v3.0
CREATE FUNCTION pgr_topologicalSort(
    TEXT, -- edges_sql (required)

    OUT seq INTEGER,
    OUT sorted_v BIGINT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_topologicalSort(_pgr_get_statement($1));
$BODY$
LANGUAGE SQL VOLATILE STRICT;


-- COMMENTS


COMMENT ON FUNCTION pgr_topologicalSort(TEXT)
IS 'pgr_topologicalSort
- EXPERIMENTAL
- Directed graph
- Parameters:
  - edges SQL with columns: id, source, target, cost [,reverse_cost]
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_topologicalSort.html
';


---------------
-- pgr_transitiveClosure
---------------


--v3.0
CREATE FUNCTION _pgr_transitiveClosure(
    edges_sql TEXT,

    OUT seq INTEGER,
    OUT vid BIGINT,
    OUT target_array BIGINT[])
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c VOLATILE STRICT;

-- COMMENTS


COMMENT ON FUNCTION _pgr_transitiveClosure(TEXT)
IS 'pgRouting internal function';


---------------
-- pgr_transitiveClosure
---------------


--v3.0
CREATE FUNCTION pgr_transitiveClosure(
    TEXT, -- edges_sql (required)

    OUT seq INTEGER,
    OUT vid BIGINT,
    OUT target_array BIGINT[])
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_transitiveClosure(_pgr_get_statement($1));
$BODY$
LANGUAGE SQL VOLATILE STRICT;


-- COMMENTS


COMMENT ON FUNCTION pgr_transitiveClosure(TEXT)
IS 'pgr_transitiveClosure
- EXPERIMENTAL
- Directed graph
- Parameters:
  - edges SQL with columns: id, source, target, cost [,reverse_cost]
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_transitiveClosure.html
';

-------------------------
-------------------------
-- _breadthFirstSearch
-------------------------
-------------------------

--v3.0
CREATE FUNCTION _pgr_breadthFirstSearch(
    edges_sql TEXT,
    from_vids ANYARRAY,
    max_depth BIGINT,
    directed  BOOLEAN,

    OUT seq BIGINT,
    OUT depth BIGINT,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c IMMUTABLE STRICT;


-- COMMENTS

COMMENT ON FUNCTION _pgr_breadthFirstSearch(TEXT, ANYARRAY, BIGINT, BOOLEAN)
IS 'pgRouting internal function';

-------------------------
-------------------------
-- _pgr_binaryBreadthFirstSearch
-------------------------
-------------------------

--v3.0
CREATE FUNCTION _pgr_binaryBreadthFirstSearch(
    edges_sql TEXT,
    from_vids ANYARRAY,
    to_vids   ANYARRAY,
    directed  BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c IMMUTABLE STRICT;


--v3.2
CREATE FUNCTION _pgr_binaryBreadthFirstSearch(
    edges_sql TEXT,
    combinations_sql TEXT,
    directed  BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE C IMMUTABLE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_binaryBreadthFirstSearch(TEXT, ANYARRAY, ANYARRAY, BOOLEAN)
IS 'pgRouting internal function';

COMMENT ON FUNCTION _pgr_binaryBreadthFirstSearch(TEXT, TEXT, BOOLEAN)
IS 'pgRouting internal function';


------------------
-- pgr_breadthFirstSearch
------------------


--ONE TO DEPTH
--v3.0
CREATE FUNCTION pgr_breadthFirstSearch(
    TEXT,   -- edges_sql (required)
    BIGINT, -- from_vid (required)

    max_depth BIGINT DEFAULT 9223372036854775807,
    directed BOOLEAN DEFAULT true,

    OUT seq BIGINT,
    OUT depth BIGINT,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
BEGIN
    IF $3 < 0 THEN
        RAISE EXCEPTION 'Negative value found on ''max_depth'''
        USING HINT = format('Value found: %s', $3);
    END IF;


    RETURN QUERY
    SELECT *
    FROM _pgr_breadthFirstSearch(_pgr_get_statement($1),  ARRAY[$2]::BIGINT[], max_depth, directed) AS a;
END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;


--MANY TO DEPTH
--v3.0
CREATE FUNCTION pgr_breadthFirstSearch(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- from_vids (required)

    max_depth BIGINT DEFAULT 9223372036854775807,
    directed BOOLEAN DEFAULT true,

    OUT seq BIGINT,
    OUT depth BIGINT,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
BEGIN
    IF $3 < 0 THEN
        RAISE EXCEPTION 'Negative value found on ''max_depth'''
        USING HINT = format('Value found: %s', $3);
    END IF;


    RETURN QUERY
    SELECT *
    FROM _pgr_breadthFirstSearch(_pgr_get_statement($1), $2::BIGINT[], max_depth, directed) AS a;
END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;


-- COMMENTS

COMMENT ON FUNCTION pgr_breadthFirstSearch(TEXT, BIGINT, BIGINT, BOOLEAN)
IS 'pgr_breadthFirstSearch(One to Depth)
- EXPERIMENTAL
- Parameters:
  - edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From vertex identifier
- Optional Parameters: 
  - Maximum Depth := 9223372036854775807
  - directed := true
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_breadthFirstSearch.html
';

COMMENT ON FUNCTION pgr_breadthFirstSearch(TEXT, ANYARRAY, BIGINT, BOOLEAN)
IS 'pgr_breadthFirstSearch(Many to Depth)
- EXPERIMENTAL
- Parameters:
  - Edges SQL with columns: id, source, target, cost [,reverse_cost]
  - From ARRAY[vertices identifiers]
- Optional Parameters
  - Maximum Depth := 9223372036854775807
  - directed := true
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_breadthFirstSearch.html
';

---------------
-- pgr_binaryBreadthFirstSearch
---------------

-- ONE to ONE
--v3.0
CREATE FUNCTION pgr_binaryBreadthFirstSearch(
    TEXT,   -- edges_sql (required)
    BIGINT, -- from_vid (required)
    BIGINT, -- to_vid (required)

    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_binaryBreadthFirstSearch(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], ARRAY[$3]::BIGINT[], $4) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT;


-- ONE to MANY
--v3.0
CREATE FUNCTION pgr_binaryBreadthFirstSearch(
    TEXT,     -- edges_sql (required)
    BIGINT,   -- from_vid (required)
    ANYARRAY, -- to_vids (required)

    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.end_vid, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_binaryBreadthFirstSearch(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], $3::BIGINT[], $4) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT;


-- MANY to ONE
--v3.0
CREATE FUNCTION pgr_binaryBreadthFirstSearch(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    BIGINT,   -- to_vid (required)

    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.start_vid, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_binaryBreadthFirstSearch(_pgr_get_statement($1), $2::BIGINT[], ARRAY[$3]::BIGINT[], $4) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT;


-- MANY to MANY
--v3.0
CREATE FUNCTION pgr_binaryBreadthFirstSearch(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- from_vids (required)
    ANYARRAY, -- to_vids (required)

    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.start_vid, a.end_vid, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_binaryBreadthFirstSearch(_pgr_get_statement($1), $2::BIGINT[], $3::BIGINT[], $4) AS a;
$BODY$
LANGUAGE sql VOLATILE STRICT;


-- COMBINATIONS
--v3.2
CREATE FUNCTION pgr_binaryBreadthFirstSearch(
    TEXT,     -- edges_sql (required)
    TEXT,     -- combinations_sql (required)

    directed BOOLEAN DEFAULT true,

    OUT seq INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT a.seq, a.path_seq, a.start_vid, a.end_vid, a.node, a.edge, a.cost, a.agg_cost
    FROM _pgr_binaryBreadthFirstSearch(_pgr_get_statement($1), _pgr_get_statement($2), $3) AS a;
$BODY$
LANGUAGE SQL VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION pgr_binaryBreadthFirstSearch(TEXT, BIGINT, BIGINT, BOOLEAN)
IS 'pgr_binaryBreadthFirstSearch(One to One)
- EXPERIMENTAL
- Parameters:
   - Edges SQL with columns: id, source, target, cost [,reverse_cost]
   - From vertex identifier
   - To vertex identifier
- Optional Parameters
   - directed := true
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_binaryBreadthFirstSearch.html
';

COMMENT ON FUNCTION pgr_binaryBreadthFirstSearch(TEXT, BIGINT, ANYARRAY, BOOLEAN)
IS 'pgr_binaryBreadthFirstSearch(One to Many)
- EXPERIMENTAL
- Parameters:
   - Edges SQL with columns: id, source, target, cost [,reverse_cost]
   - From vertex identifier
   - To ARRAY[vertices identifiers]
- Optional Parameters
   - directed := true
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_binaryBreadthFirstSearch.html
';

COMMENT ON FUNCTION pgr_binaryBreadthFirstSearch(TEXT, ANYARRAY, BIGINT, BOOLEAN)
IS 'pgr_binaryBreadthFirstSearch(Many to One)
- EXPERIMENTAL
- Parameters:
   - Edges SQL with columns: id, source, target, cost [,reverse_cost]
   - From ARRAY[vertices identifiers]
   - To vertex identifier
- Optional Parameters
   - directed := true
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_binaryBreadthFirstSearch.html
';

COMMENT ON FUNCTION pgr_binaryBreadthFirstSearch(TEXT, ANYARRAY, ANYARRAY, BOOLEAN)
IS 'pgr_binaryBreadthFirstSearch(Many to Many)
- EXPERIMENTAL
- Parameters:
   - Edges SQL with columns: id, source, target, cost [,reverse_cost]
   - From ARRAY[vertices identifiers]
   - To ARRAY[vertices identifiers]
- Optional Parameters
   - directed := true
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_binaryBreadthFirstSearch.html
';

COMMENT ON FUNCTION pgr_binaryBreadthFirstSearch(TEXT, TEXT, BOOLEAN)
IS 'pgr_binaryBreadthFirstSearch(Combinations)
- EXPERIMENTAL
- Parameters:
   - Edges SQL with columns: id, source, target, cost [,reverse_cost]
   - Combinations SQL with columns: source, target
- Optional Parameters
   - directed := true
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_binaryBreadthFirstSearch.html
';


---------------------------
-- _pgr_depthFirstSearch
---------------------------


--v3.2
CREATE FUNCTION _pgr_depthFirstSearch(
    edges_sql TEXT,
    root_vids ANYARRAY,

    directed BOOLEAN,
    max_depth BIGINT,

    OUT seq BIGINT,
    OUT depth BIGINT,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE C VOLATILE STRICT;


-- COMMENTS


COMMENT ON FUNCTION _pgr_depthFirstSearch(TEXT, ANYARRAY, BOOLEAN, BIGINT)
IS 'pgRouting internal function';



--------------------------
-- pgr_depthFirstSearch
--------------------------


-- SINGLE VERTEX
--v3.2
CREATE FUNCTION pgr_depthFirstSearch(
    TEXT,   -- edges_sql (required)
    BIGINT, -- root_vid (required)

    directed BOOLEAN DEFAULT true,
    max_depth BIGINT DEFAULT 9223372036854775807,

    OUT seq BIGINT,
    OUT depth BIGINT,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
BEGIN
    IF $4 < 0 THEN
        RAISE EXCEPTION 'Negative value found on ''max_depth'''
        USING HINT = format('Value found: %s', $4);
    END IF;


    RETURN QUERY
    SELECT *
    FROM _pgr_depthFirstSearch(_pgr_get_statement($1), ARRAY[$2]::BIGINT[], directed, max_depth);
END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;


-- MULTIPLE VERTICES
--v3.2
CREATE FUNCTION pgr_depthFirstSearch(
    TEXT,     -- edges_sql (required)
    ANYARRAY, -- root_vids (required)

    directed BOOLEAN DEFAULT true,
    max_depth BIGINT DEFAULT 9223372036854775807,

    OUT seq BIGINT,
    OUT depth BIGINT,
    OUT start_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)
RETURNS SETOF RECORD AS
$BODY$
BEGIN
    IF $4 < 0 THEN
        RAISE EXCEPTION 'Negative value found on ''max_depth'''
        USING HINT = format('Value found: %s', $4);
    END IF;


    RETURN QUERY
    SELECT *
    FROM _pgr_depthFirstSearch(_pgr_get_statement($1), $2, directed, max_depth);
END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;


-- COMMENTS


COMMENT ON FUNCTION pgr_depthFirstSearch(TEXT, BIGINT, BOOLEAN, BIGINT)
IS 'pgr_depthFirstSearch(Single Vertex)
- PROPOSED
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - From root vertex identifier
- Optional parameters
    - directed := true
    - max_depth := 9223372036854775807
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_depthFirstSearch.html
';

COMMENT ON FUNCTION pgr_depthFirstSearch(TEXT, ANYARRAY, BOOLEAN, BIGINT)
IS 'pgr_depthFirstSearch(Multiple Vertices)
- PROPOSED
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
    - From ARRAY[root vertices identifiers]
- Optional parameters
    - directed := true
    - max_depth := 9223372036854775807
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_depthFirstSearch.html
';


----------------------------------
-- _pgr_sequentialVertexColoring
----------------------------------


--v3.2
CREATE FUNCTION _pgr_sequentialVertexColoring(
    edges_sql TEXT,

    OUT vertex_id BIGINT,
    OUT color_id BIGINT)

RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE C IMMUTABLE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_sequentialVertexColoring(TEXT)
IS 'pgRouting internal function';


----------------------------------
-- pgr_sequentialVertexColoring
----------------------------------


--v3.2
CREATE FUNCTION pgr_sequentialVertexColoring(
    TEXT,    -- edges_sql (required)

    OUT vertex_id BIGINT,
    OUT color_id BIGINT)
RETURNS SETOF RECORD AS
$BODY$
BEGIN
    RETURN QUERY
    SELECT *
    FROM _pgr_sequentialVertexColoring(_pgr_get_statement($1));
END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION pgr_sequentialVertexColoring(TEXT)
IS 'pgr_sequentialVertexColoring
- PROPOSED
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_sequentialVertexColoring.html
';

---------------
-- _pgr_bipartite
---------------

--v3.2
CREATE FUNCTION _pgr_bipartite(
    edges_sql TEXT,

    OUT node BIGINT,
    OUT color BIGINT)

RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE C IMMUTABLE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_bipartite(TEXT)
IS 'pgRouting internal function';

---------------
-- pgr_bipartite
---------------

--v3.2
CREATE FUNCTION pgr_bipartite(
    TEXT, -- edges_sql (required)

    OUT vertex_id BIGINT,
    OUT color_id BIGINT)
RETURNS SETOF RECORD AS
$BODY$
BEGIN
    RETURN QUERY
    SELECT *
    FROM _pgr_bipartite(_pgr_get_statement($1));
END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION pgr_bipartite(TEXT)
IS 'pgr_bipartite
- EXPERIMENTAL
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_bipartite.html
';

---------------
-- _pgr_edgeColoring
---------------

--v3.3
CREATE FUNCTION _pgr_edgeColoring(
    edges_sql TEXT,

    OUT edge_id BIGINT,
    OUT color_id BIGINT)

RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE C IMMUTABLE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_edgeColoring(TEXT)
IS 'pgRouting internal function';

---------------
-- pgr_edgeColoring
---------------

--v3.3
CREATE FUNCTION pgr_edgeColoring(
    TEXT, -- edges_sql (required)

    OUT edge_id BIGINT,
    OUT color_id BIGINT)
RETURNS SETOF RECORD AS
$BODY$
BEGIN
    RETURN QUERY
    SELECT *
    FROM _pgr_edgeColoring(_pgr_get_statement($1));
END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION pgr_edgeColoring(TEXT)
IS 'pgr_edgeColoring
- EXPERIMENTAL
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_edgeColoring.html
';

-------------------------
-------------------------
-- _isPlanar
-------------------------
-------------------------

--v3.2
CREATE FUNCTION _pgr_isPlanar(
  TEXT   -- edges_sql (required)
     )

RETURNS BOOLEAN AS
'MODULE_PATHNAME'
LANGUAGE c IMMUTABLE STRICT;


-- COMMENTS

COMMENT ON FUNCTION _pgr_isPlanar(TEXT)
IS 'pgRouting internal function';


------------------
-- pgr_isPlanar
------------------

--v3.2
CREATE FUNCTION pgr_isPlanar(
    TEXT   -- edges_sql (required)
        )

RETURNS BOOLEAN AS
$BODY$
    SELECT *
    FROM _pgr_isPlanar(_pgr_get_statement($1)) AS a;
$BODY$
LANGUAGE SQL VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION pgr_isPlanar(TEXT)
IS 'pgr_isPlanar
- EXPERIMENTAL
- Undirected graph
- Parameters:
  - edges SQL with columns: id, source, target, cost [,reverse_cost]
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_isPlanar.html
';

---------------
-- _pgr_lengauerTarjanDominatorTree
---------------
--v3.2
CREATE FUNCTION _pgr_lengauerTarjanDominatorTree (
    edges_sql TEXT, -- edges_sql (required)
    root_vid BIGINT , -- vertex (required)
    OUT seq integer,
    OUT vid BIGINT,
    OUT idom BIGINT)

RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE c VOLATILE STRICT;

-- COMMENTS


COMMENT ON FUNCTION _pgr_lengauerTarjanDominatorTree (TEXT,BIGINT)
IS 'pgRouting internal function';


---------------
-- pgr_lengauerTarjanDominatorTree
---------------
--v3.2
CREATE FUNCTION pgr_lengauerTarjanDominatorTree(
    TEXT,   -- edges_sql (required)
    BIGINT, -- start_vid (required)
    OUT seq integer,
    OUT vertex_id BIGINT,
    OUT idom BIGINT
    )
RETURNS SETOF RECORD AS
$BODY$
BEGIN

    RETURN QUERY
    SELECT *
    FROM _pgr_lengauerTarjanDominatorTree(_pgr_get_statement($1),$2);
END;
$BODY$
LANGUAGE  plpgsql VOLATILE STRICT;


-- COMMENTS


COMMENT ON FUNCTION pgr_lengauerTarjanDominatorTree(TEXT,BIGINT)
IS 'pgr_lengauerTarjanDominatorTree
- EXPERIMENTAL
- Directed graph
- Parameters:
  - edges SQL with columns: id, source, target, cost [,reverse_cost]
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_lengauerTarjanDominatorTree.html
';



----------------------------
-- _pgr_cuthillMckeeOrdering
----------------------------

--v3.4.0
CREATE FUNCTION _pgr_cuthillMckeeOrdering(
    TEXT,
    OUT seq BIGINT,
    OUT node BIGINT
    )

RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE C IMMUTABLE STRICT;

-- COMMENTS

COMMENT ON FUNCTION _pgr_cuthillMckeeOrdering(TEXT)
IS 'pgRouting internal function';



----------------------------
-- pgr_cuthillMckeeOrdering
----------------------------

--v3.4.0
CREATE FUNCTION pgr_cuthillMckeeOrdering(
    TEXT, -- edges_sql (required)
    OUT seq BIGINT,
    OUT node BIGINT)
RETURNS SETOF RECORD AS
$BODY$
    SELECT *
    FROM _pgr_cuthillMckeeOrdering(_pgr_get_statement($1));
$BODY$
LANGUAGE SQL VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION pgr_cuthillMckeeOrdering(TEXT)
IS 'pgr_cuthillMckeeOrdering
- EXPERIMENTAL
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_cuthillMckeeOrdering.html
';

---------------
-- _pgr_hawickCircuits
---------------

--v3.4.0
CREATE FUNCTION _pgr_hawickCircuits(
    TEXT,

    OUT seq INTEGER,
    OUT path_id  INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
'MODULE_PATHNAME'
LANGUAGE C IMMUTABLE STRICT;

COMMENT ON FUNCTION _pgr_hawickCircuits(TEXT)
IS 'pgRouting internal function';

---------------
-- pgr_hawickCircuits
---------------

--v3.4.0
CREATE FUNCTION pgr_hawickCircuits(
    TEXT, -- edges_sql (required)

    OUT seq INTEGER,
    OUT path_id INTEGER,
    OUT path_seq INTEGER,
    OUT start_vid BIGINT,
    OUT end_vid BIGINT,
    OUT node BIGINT,
    OUT edge BIGINT,
    OUT cost FLOAT,
    OUT agg_cost FLOAT)

RETURNS SETOF RECORD AS
$BODY$
BEGIN
    RETURN QUERY
    SELECT *
    FROM _pgr_hawickCircuits(_pgr_get_statement($1));
END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;

COMMENT ON FUNCTION pgr_hawickCircuits(TEXT)
IS 'pgr_hawickCircuits
- EXPERIMENTAL
- Parameters:
    - Edges SQL with columns: id, source, target, cost [,reverse_cost]
- Documentation:
    - https://docs.pgrouting.org/latest/en/pgr_hawickCircuits.html
';




---------------
---------------
-- topology
---------------
---------------


-----------------------
-- pgr_createtopology
-----------------------


--v2.6
CREATE FUNCTION pgr_createTopology(
    TEXT, -- edge table (required)
    double precision, -- tolerance (required)
    the_geom TEXT default 'the_geom',
    id TEXT default 'id',
    source TEXT default 'source',
    target TEXT default 'target',
    rows_where TEXT default 'true',
    clean boolean default FALSE)
RETURNS VARCHAR AS
$BODY$

DECLARE
    edge_table TEXT := $1;
    tolerance FLOAT := $2;
    points record;
    sridinfo record;
    source_id BIGINT;
    target_id BIGINT;
    totcount BIGINT;
    rowcount BIGINT;
    srid INTEGER;
    sql TEXT;
    sname TEXT;
    tname TEXT;
    tabname TEXT;
    vname TEXT;
    vertname TEXT;
    gname TEXT;
    idname TEXT;
    sourcename TEXT;
    targetname TEXT;
    notincluded INTEGER;
    i INTEGER;
    naming record;
    info record;
    flag boolean;
    query TEXT;
    idtype TEXT;
    gtype TEXT;
    sourcetype TEXT;
    targettype TEXT;
    debuglevel TEXT;
    dummyRec record;
    fnName TEXT;
    err bool;
    msgKind int;
    emptied BOOLEAN;

BEGIN
    msgKind = 1; -- notice
    fnName = 'pgr_createTopology';
    RAISE notice 'PROCESSING:';
    RAISE notice 'pgr_createTopology(''%'', %, ''%'', ''%'', ''%'', ''%'', rows_where := ''%'', clean := %)',edge_table,tolerance,the_geom,id,source,target,rows_where, clean;
    EXECUTE 'show client_min_messages' INTO debuglevel;


    RAISE notice 'Performing checks, please wait .....';

        EXECUTE 'SELECT * FROM _pgr_getTableName('|| quote_literal(edge_table)
                                                  || ',2,' || quote_literal(fnName) ||' )' INTO naming;
        sname=naming.sname;
        tname=naming.tname;
        tabname=sname||'.'||tname;
        vname=tname||'_vertices_pgr';
        vertname= sname||'.'||vname;
        rows_where = ' AND ('||rows_where||')';
      RAISE DEBUG '     --> OK';


      RAISE debug 'Checking column names in edge table';
        SELECT * INTO idname     FROM _pgr_getColumnName(sname, tname,id,2,fnName);
        SELECT * INTO sourcename FROM _pgr_getColumnName(sname, tname,source,2,fnName);
        SELECT * INTO targetname FROM _pgr_getColumnName(sname, tname,target,2,fnName);
        SELECT * INTO gname      FROM _pgr_getColumnName(sname, tname,the_geom,2,fnName);


        err = sourcename in (targetname,idname,gname) OR  targetname in (idname,gname) OR idname=gname;
        perform _pgr_onError( err, 2, fnName,
               'Two columns share the same name', 'Parameter names for id,the_geom,source and target  must be different',
	       'Column names are OK');

      RAISE DEBUG '     --> OK';

      RAISE debug 'Checking column types in edge table';
        SELECT * INTO sourcetype FROM _pgr_getColumnType(sname,tname,sourcename,1, fnName);
        SELECT * INTO targettype FROM _pgr_getColumnType(sname,tname,targetname,1, fnName);
        SELECT * INTO idtype FROM _pgr_getColumnType(sname,tname,idname,1, fnName);

        err = idtype NOT in('integer','smallint','bigint');
        perform _pgr_onError(err, 2, fnName,
	       'Wrong type of Column id:'|| idname, ' Expected type of '|| idname || ' is integer,smallint or bigint but '||idtype||' was found');

        err = sourcetype NOT in('integer','smallint','bigint');
        perform _pgr_onError(err, 2, fnName,
	       'Wrong type of Column source:'|| sourcename, ' Expected type of '|| sourcename || ' is integer,smallint or bigint but '||sourcetype||' was found');

        err = targettype NOT in('integer','smallint','bigint');
        perform _pgr_onError(err, 2, fnName,
	       'Wrong type of Column target:'|| targetname, ' Expected type of '|| targetname || ' is integer,smallint or bigint but '||targettype||' was found');

      RAISE DEBUG '     --> OK';

      RAISE debug 'Checking SRID of geometry column';
         query= 'SELECT ST_SRID(' || quote_ident(gname) || ') AS srid '
            || ' FROM ' || _pgr_quote_ident(tabname)
            || ' WHERE ' || quote_ident(gname)
            || ' IS NOT NULL LIMIT 1';
         RAISE debug '%',query;
         EXECUTE query INTO sridinfo;

         err =  sridinfo IS NULL OR sridinfo.srid IS NULL;
         perform _pgr_onError(err, 2, fnName,
	     'Can not determine the srid of the geometry '|| gname ||' in table '||tabname, 'Check the geometry of column '||gname);

         srid := sridinfo.srid;
      RAISE DEBUG '     --> OK';

      RAISE debug 'Checking and creating indices in edge table';
        perform _pgr_createIndex(sname, tname , idname , 'btree'::TEXT);
        perform _pgr_createIndex(sname, tname , sourcename , 'btree'::TEXT);
        perform _pgr_createIndex(sname, tname , targetname , 'btree'::TEXT);
        perform _pgr_createIndex(sname, tname , gname , 'gist'::TEXT);

        gname=quote_ident(gname);
        idname=quote_ident(idname);
        sourcename=quote_ident(sourcename);
        targetname=quote_ident(targetname);
      RAISE DEBUG '     --> OK';





    BEGIN
        -- issue #193 & issue #210 & #213
        -- this sql is for trying out the where clause
        -- the select * is to avoid any column name conflicts
        -- limit 1, just try on first record
        -- if the where clasuse is ill formed it will be caught in the exception
        sql = 'SELECT * FROM '||_pgr_quote_ident(tabname)||' WHERE true'||rows_where ||' limit 1';
        EXECUTE sql INTO dummyRec;
        -- end

        -- if above where clasue works this one should work
        -- any error will be caught by the exception also
        sql = 'SELECT count(*) FROM '||_pgr_quote_ident(tabname)||' WHERE (' || gname || ' IS NOT NULL AND '||
	    idname||' IS NOT NULL)=false '||rows_where;
        EXECUTE SQL  INTO notincluded;

        if clean then
            RAISE debug 'Cleaning previous Topology ';
               EXECUTE 'UPDATE ' || _pgr_quote_ident(tabname) ||
               ' SET '||sourcename||' = NULL,'||targetname||' = NULL';
        else
            RAISE debug 'Creating topology for edges with non assigned topology';
            if rows_where=' AND (true)' then
                rows_where=  ' AND ('||quote_ident(sourcename)||' is NULL OR '||quote_ident(targetname)||' is  NULL)';
            end if;
        end if;
        -- my thoery is that the select Count(*) will never go through here
        EXCEPTION WHEN OTHERS THEN
             RAISE NOTICE 'Got %', SQLERRM; -- issue 210,211
             RAISE NOTICE 'ERROR: Condition is not correct, please execute the following query to test your condition';
             RAISE NOTICE '%',sql;
             RETURN 'FAIL';
    END;

    BEGIN
         RAISE DEBUG 'initializing %',vertname;
         EXECUTE 'SELECT * FROM _pgr_getTableName('||quote_literal(vertname)
                                                  || ',0,' || quote_literal(fnName) ||' )' INTO naming;
         emptied = false;
         set client_min_messages  to warning;
         IF sname=naming.sname AND vname=naming.tname  THEN
            if clean then
                EXECUTE 'TRUNCATE TABLE '||_pgr_quote_ident(vertname)||' RESTART IDENTITY';
                EXECUTE 'SELECT DROPGEOMETRYCOLUMN('||quote_literal(sname)||','||quote_literal(vname)||','||quote_literal('the_geom')||')';
                emptied = true;
            end if;
         ELSE -- table doesn't exist
            EXECUTE 'CREATE TABLE '||_pgr_quote_ident(vertname)||' (id bigserial PRIMARY KEY,cnt integer,chk integer,ein integer,eout integer)';
            emptied = true;
         END IF;
         IF (emptied) THEN
             EXECUTE 'SELECT addGeometryColumn('||quote_literal(sname)||','||quote_literal(vname)||','||
	         quote_literal('the_geom')||','|| srid||', '||quote_literal('POINT')||', 2)';
             perform _pgr_createIndex(vertname , 'the_geom'::TEXT , 'gist'::TEXT);
         END IF;
         EXECUTE 'SELECT * FROM  _pgr_checkVertTab('||quote_literal(vertname) ||', ''{"id"}''::TEXT[])' INTO naming;
         EXECUTE 'set client_min_messages  to '|| debuglevel;
         RAISE DEBUG  '  ------>OK';
         EXCEPTION WHEN OTHERS THEN
             RAISE NOTICE 'Got %', SQLERRM; -- issue 210,211
             RAISE NOTICE 'ERROR: something went wrong when initializing the verties table';
             RETURN 'FAIL';
    END;



    RAISE notice 'Creating Topology, Please wait...';
        rowcount := 0;
        FOR points IN EXECUTE 'SELECT ' || idname || '::BIGINT AS id,'
            || ' _pgr_StartPoint(' || gname || ') AS source,'
            || ' _pgr_EndPoint('   || gname || ') AS target'
            || ' FROM '  || _pgr_quote_ident(tabname)
            || ' WHERE ' || gname || ' IS NOT NULL AND ' || idname||' IS NOT NULL '||rows_where
        LOOP

            rowcount := rowcount + 1;
            IF rowcount % 1000 = 0 THEN
                RAISE NOTICE '% edges processed', rowcount;
            END IF;


            source_id := _pgr_pointToId(points.source, tolerance,vertname,srid);
            target_id := _pgr_pointToId(points.target, tolerance,vertname,srid);
            BEGIN
                sql := 'UPDATE ' || _pgr_quote_ident(tabname) ||
                    ' SET '||sourcename||' = '|| source_id::TEXT || ','||targetname||' = ' || target_id::TEXT ||
                    ' WHERE ' || idname || ' =  ' || points.id::TEXT;

                IF sql IS NULL THEN
                    RAISE NOTICE 'WARNING: UPDATE % SET source = %, target = % WHERE % = % ', tabname, source_id::TEXT, target_id::TEXT, idname,  points.id::TEXT;
                ELSE
                    EXECUTE sql;
                END IF;
                EXCEPTION WHEN OTHERS THEN
                    RAISE NOTICE '%', SQLERRM;
                    RAISE NOTICE '%',sql;
                    RETURN 'FAIL';
            end;
        END LOOP;
        RAISE notice '-------------> TOPOLOGY CREATED FOR  % edges', rowcount;
        RAISE NOTICE 'Rows with NULL geometry or NULL id: %',notincluded;
        RAISE notice 'Vertices table for table % is: %',_pgr_quote_ident(tabname), _pgr_quote_ident(vertname);
        RAISE notice '----------------------------------------------';

    RETURN 'OK';
 EXCEPTION WHEN OTHERS THEN
   RAISE NOTICE 'Unexpected error %', SQLERRM; -- issue 210,211
   RETURN 'FAIL';
END;


$BODY$
LANGUAGE plpgsql VOLATILE STRICT;


-- COMMENTS


COMMENT ON FUNCTION pgr_createTopology(TEXT, FLOAT, TEXT, TEXT, TEXT, TEXT, TEXT, BOOLEAN)
IS 'pgr_createTopology
 - Parameters
   - Edge table name
   - tolerance
 - Optional parameters
   - the_geom := ''the_geom''
   - id := ''id''
   - source := ''source''
   - target := ''target''
   - rows_where := ''true''
   - clean := false
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_createTopology.html
';






-----------------------
-- pgr_analyzegraph
-----------------------


--v2.6
CREATE FUNCTION pgr_analyzeGraph(
    TEXT, -- edge table (required)
    double precision, -- tolerance (required)

    the_geom TEXT default 'the_geom',
    id TEXT default 'id',
    source TEXT default 'source',
    target TEXT default 'target',
    rows_where TEXT default 'true')

RETURNS character varying AS
$BODY$

DECLARE
    edge_table TEXT := $1;
    tolerance TEXT := $2;
    points record;
    seg record;
    naming record;
    sridinfo record;
    srid INTEGER;
    ecnt INTEGER;
    vertname TEXT;
    sname TEXT;
    tname TEXT;
    vname TEXT;
    idname TEXT;
    sourcename TEXT;
    targetname TEXT;
    sourcetype TEXT;
    targettype TEXT;
    geotype TEXT;
    gname TEXT;
    tabName TEXT;
    flag boolean ;
    query TEXT;
    selectionquery TEXT;
    i INTEGER;
    tot INTEGER;
    NumIsolated INTEGER;
    numdeadends INTEGER;
    numgaps INTEGER;
    NumCrossing INTEGER;
    numRings INTEGER;
    debuglevel TEXT;




BEGIN
  RAISE NOTICE 'PROCESSING:';
  RAISE NOTICE 'pgr_analyzeGraph(''%'',%,''%'',''%'',''%'',''%'',''%'')',edge_table,tolerance,the_geom,id,source,target,rows_where;
  RAISE NOTICE 'Performing checks, please wait ...';
  EXECUTE 'show client_min_messages' INTO debuglevel;


  BEGIN
    RAISE DEBUG 'Checking % exists',edge_table;
    EXECUTE 'select * FROM _pgr_getTableName('||quote_literal(edge_table)||',2)' INTO naming;
    sname=naming.sname;
    tname=naming.tname;
    tabname=sname||'.'||tname;
    vname=tname||'_vertices_pgr';
    vertname= sname||'.'||vname;
    rows_where = ' AND ('||rows_where||')';
    RAISE DEBUG '     --> OK';

  END;

  BEGIN
       RAISE DEBUG 'Checking Vertices table';
       EXECUTE 'select * FROM  _pgr_checkVertTab('||quote_literal(vertname) ||', ''{"id","cnt","chk"}''::TEXT[])' INTO naming;
       EXECUTE 'UPDATE '||_pgr_quote_ident(vertname)||' SET cnt=0 ,chk=0';
       RAISE DEBUG '     --> OK';
       EXCEPTION WHEN raise_exception THEN
          RAISE NOTICE 'ERROR: something went wrong checking the vertices table';
          RETURN 'FAIL';
  END;



  BEGIN
       RAISE DEBUG 'Checking column names in edge table';
       SELECT * INTO idname     FROM _pgr_getColumnName(sname, tname,id,2);
       SELECT * INTO sourcename FROM _pgr_getColumnName(sname, tname,source,2);
       SELECT * INTO targetname FROM _pgr_getColumnName(sname, tname,target,2);
       SELECT * INTO gname      FROM _pgr_getColumnName(sname, tname,the_geom,2);


       perform _pgr_onError( sourcename IN (targetname,idname,gname) OR  targetname IN (idname,gname) OR idname=gname, 2,
                       'pgr_analyzeGraph',  'Two columns share the same name', 'Parameter names for id,the_geom,source and target  must be different',
                       'Column names are OK');

        RAISE DEBUG '     --> OK';
       EXCEPTION WHEN raise_exception THEN
          RAISE NOTICE 'ERROR: something went wrong checking the column names';
          RETURN 'FAIL';
  END;


  BEGIN
       RAISE DEBUG 'Checking column types in edge table';
       SELECT * INTO sourcetype FROM _pgr_getColumnType(sname,tname,sourcename,1);
       SELECT * INTO targettype FROM _pgr_getColumnType(sname,tname,targetname,1);

       perform _pgr_onError(sourcetype NOT in('integer','smallint','bigint') , 2,
                       'pgr_analyzeGraph',  'Wrong type of Column '|| sourcename, ' Expected type of '|| sourcename || ' is integer, smallint or bigint but '||sourcetype||' was found',
                       'Type of Column '|| sourcename || ' is ' || sourcetype);

       perform _pgr_onError(targettype NOT in('integer','smallint','bigint') , 2,
                       'pgr_analyzeGraph',  'Wrong type of Column '|| targetname, ' Expected type of '|| targetname || ' is integer, smallint or bigint but '||targettype||' was found',
                       'Type of Column '|| targetname || ' is ' || targettype);

       RAISE DEBUG '     --> OK';
       EXCEPTION WHEN raise_exception THEN
          RAISE NOTICE 'ERROR: something went wrong checking the column types';
          RETURN 'FAIL';
   END;

   BEGIN
       RAISE DEBUG 'Checking SRID of geometry column';
         query= 'SELECT ST_SRID(' || quote_ident(gname) || ') AS srid '
            || ' FROM ' || _pgr_quote_ident(tabname)
            || ' WHERE ' || quote_ident(gname)
            || ' IS NOT NULL LIMIT 1';
         EXECUTE QUERY INTO sridinfo;

         perform _pgr_onError( sridinfo IS NULL OR sridinfo.srid IS NULL,2,
                 'Can not determine the srid of the geometry '|| gname ||' in table '||tabname, 'Check the geometry of column '||gname,
                 'SRID of '||gname||' is '||sridinfo.srid);

         IF sridinfo IS NULL OR sridinfo.srid IS NULL THEN
             RAISE NOTICE ' Can not determine the srid of the geometry "%" in table %', the_geom,tabname;
             RETURN 'FAIL';
         END IF;
         srid := sridinfo.srid;
         RAISE DEBUG '     --> OK';
         EXCEPTION WHEN OTHERS THEN
             RAISE NOTICE 'Got %', SQLERRM;--issue 210,211,213
             RAISE NOTICE 'ERROR: something went wrong when checking for SRID of % in table %', the_geom,tabname;
             RETURN 'FAIL';
    END;


    BEGIN
       RAISE DEBUG 'Checking  indices in edge table';
       perform _pgr_createIndex(tabname , idname , 'btree');
       perform _pgr_createIndex(tabname , sourcename , 'btree');
       perform _pgr_createIndex(tabname , targetname , 'btree');
       perform _pgr_createIndex(tabname , gname , 'gist');

       gname=quote_ident(gname);
       sourcename=quote_ident(sourcename);
       targetname=quote_ident(targetname);
       idname=quote_ident(idname);
       RAISE DEBUG '     --> OK';
       EXCEPTION WHEN raise_exception THEN
          RAISE NOTICE 'ERROR: something went wrong checking indices';
          RETURN 'FAIL';
    END;


    BEGIN
        query='select count(*) from '||_pgr_quote_ident(tabname)||' WHERE true  '||rows_where;
        EXECUTE query INTO ecnt;
        RAISE DEBUG '-->Rows WHERE condition: OK';
        RAISE DEBUG '     --> OK';
         EXCEPTION WHEN OTHERS THEN
            RAISE NOTICE 'Got %', SQLERRM;  --issue 210,211,213
            RAISE NOTICE 'ERROR: Condition is not correct. Please execute the following query to test your condition';
            RAISE NOTICE '%',query;
            RETURN 'FAIL';
    END;

    selectionquery ='with
           selectedRows as( (select '||sourcename||' AS id FROM '||_pgr_quote_ident(tabname)||' WHERE true '||rows_where||')
                           UNION
                           (select '||targetname||' AS id FROM '||_pgr_quote_ident(tabname)||' WHERE true '||rows_where||'))';





   BEGIN
       RAISE NOTICE 'Analyzing for dead ends. Please wait...';
       query= 'with countingsource AS (select a.'||sourcename||' AS id,count(*) AS cnts
               FROM (select * FROM '||_pgr_quote_ident(tabname)||' WHERE true '||rows_where||' ) a  GROUP BY a.'||sourcename||')
                     ,countingtarget AS (select a.'||targetname||' AS id,count(*) AS cntt
                    FROM (select * FROM '||_pgr_quote_ident(tabname)||' WHERE true '||rows_where||' ) a  GROUP BY a.'||targetname||')
                   ,totalcount AS (select id,case when cnts is NULL AND cntt is NULL then 0
                                                   when cnts is NULL then cntt
                                                   when cntt is NULL then cnts
                                                   else cnts+cntt end as totcnt
                                   FROM ('||_pgr_quote_ident(vertname)||' AS a left
                                   join countingsource AS t using(id) ) left join countingtarget using(id))
               UPDATE '||_pgr_quote_ident(vertname)||' AS a set cnt=totcnt FROM totalcount AS b WHERE a.id=b.id';
       RAISE DEBUG '%',query;
       EXECUTE query;
       query=selectionquery||'
              select count(*)  FROM '||_pgr_quote_ident(vertname)||' WHERE cnt=1 AND id IN (select id FROM selectedRows)';
       RAISE DEBUG '%',query;
       EXECUTE query  INTO numdeadends;
       RAISE DEBUG '     --> OK';
       EXCEPTION WHEN raise_exception THEN
          RAISE NOTICE 'Got %', SQLERRM;  --issue 210,211,213
          RAISE NOTICE 'ERROR: something went wrong when analizing for dead ends';
          RETURN 'FAIL';
   END;



    BEGIN
          RAISE NOTICE 'Analyzing for gaps. Please wait...';
          query = 'with
                   buffer AS (select id,st_buffer(the_geom,'||tolerance||') AS buff FROM '||_pgr_quote_ident(vertname)||' WHERE cnt=1)
                   ,veryclose AS (select b.id,st_crosses(a.'||gname||',b.buff) AS flag
                   FROM  (select * FROM '||_pgr_quote_ident(tabname)||' WHERE true '||rows_where||' ) AS a
                   join buffer AS b on (a.'||gname||'&&b.buff)
                   WHERE '||sourcename||'!=b.id AND '||targetname||'!=b.id )
                   UPDATE '||_pgr_quote_ident(vertname)||' set chk=1 WHERE id IN (select distinct id FROM veryclose WHERE flag=true)';
          RAISE DEBUG '%' ,query;
          EXECUTE query;
          GET DIAGNOSTICS  numgaps= ROW_COUNT;
          RAISE DEBUG '     --> OK';
          EXCEPTION WHEN raise_exception THEN
            RAISE NOTICE 'ERROR: something went wrong when Analyzing for gaps';
            RETURN 'FAIL';
    END;

    BEGIN
        RAISE NOTICE 'Analyzing for isolated edges. Please wait...';
        query=selectionquery|| ' select count(*) FROM (select * FROM '||_pgr_quote_ident(tabname)||' WHERE true '||rows_where||' )  AS a,
                                                 '||_pgr_quote_ident(vertname)||' AS b,
                                                 '||_pgr_quote_ident(vertname)||' AS c
                            WHERE b.id IN (select id FROM selectedRows) AND a.'||sourcename||' =b.id
                            AND b.cnt=1 AND a.'||targetname||' =c.id
                            AND c.cnt=1';
        RAISE DEBUG '%' ,query;
        EXECUTE query  INTO NumIsolated;
        RAISE DEBUG '     --> OK';
        EXCEPTION WHEN raise_exception THEN
            RAISE NOTICE 'ERROR: something went wrong when Analyzing for isolated edges';
            RETURN 'FAIL';
    END;

    BEGIN
        RAISE NOTICE 'Analyzing for ring geometries. Please wait...';
        EXECUTE 'select geometrytype('||gname||')  FROM '||_pgr_quote_ident(tabname) limit 1 INTO geotype;
        IF (geotype='MULTILINESTRING') THEN
            query ='select count(*)  FROM '||_pgr_quote_ident(tabname)||'
                                 WHERE true  '||rows_where||' AND st_isRing(st_linemerge('||gname||'))';
            RAISE DEBUG '%' ,query;
            EXECUTE query  INTO numRings;
        ELSE query ='select count(*)  FROM '||_pgr_quote_ident(tabname)||'
                                  WHERE true  '||rows_where||' AND st_isRing('||gname||')';
            RAISE DEBUG '%' ,query;
            EXECUTE query  INTO numRings;
        END IF;
        RAISE DEBUG '     --> OK';
        EXCEPTION WHEN raise_exception THEN
            RAISE NOTICE 'ERROR: something went wrong when Analyzing for ring geometries';
            RETURN 'FAIL';
    END;

    BEGIN
        RAISE NOTICE 'Analyzing for intersections. Please wait...';
        query = 'select count(*) FROM (select distinct case when a.'||idname||' < b.'||idname||' then a.'||idname||'
                                                        else b.'||idname||' end,
                                                   case when a.'||idname||' < b.'||idname||' then b.'||idname||'
                                                        else a.'||idname||' end
                                    FROM (select * FROM '||_pgr_quote_ident(tabname)||' WHERE true '||rows_where||') AS a
                                    JOIN (select * FROM '||_pgr_quote_ident(tabname)||' WHERE true '||rows_where||') AS b
                                    ON (a.'|| gname||' && b.'||gname||')
                                    WHERE a.'||idname||' != b.'||idname|| '
                                        AND (a.'||sourcename||' IN (b.'||sourcename||',b.'||targetname||')
                                              OR a.'||targetname||' IN (b.'||sourcename||',b.'||targetname||')) = false
                                        AND st_intersects(a.'||gname||', b.'||gname||')=true) AS d ';
        RAISE DEBUG '%' ,query;
        EXECUTE query  INTO numCrossing;
        RAISE DEBUG '     --> OK';
        EXCEPTION WHEN raise_exception THEN
            RAISE NOTICE 'ERROR: something went wrong when Analyzing for intersections';
            RETURN 'FAIL';
    END;




    RAISE NOTICE '            ANALYSIS RESULTS FOR SELECTED EDGES:';
    RAISE NOTICE '                  Isolated segments: %', NumIsolated;
    RAISE NOTICE '                          Dead ends: %', numdeadends;
    RAISE NOTICE 'Potential gaps found near dead ends: %', numgaps;
    RAISE NOTICE '             Intersections detected: %',numCrossing;
    RAISE NOTICE '                    Ring geometries: %',numRings;


    RETURN 'OK';
END;
$BODY$
  LANGUAGE plpgsql VOLATILE STRICT;



-- COMMENTS


COMMENT ON FUNCTION pgr_analyzeGraph(TEXT, FLOAT, TEXT, TEXT, TEXT, TEXT, TEXT)
IS 'pgr_analyzeGraph
- Parameters
  - Edge table name
  - tolerance
- Optional parameters
  - the_geom: default ''the_geom''
  - id := ''id''
  - source := ''source''
  - target := ''target''
  - rows_where := ''true''
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_analyzeGraph.html
';





--v2.6
CREATE FUNCTION pgr_analyzeOneway(
   TEXT,
   TEXT[], -- s_in_rules (required)
   TEXT[], -- s_out_rules (required)
   TEXT[], -- t_in_rules (required)
   TEXT[], -- t_out_rules (required)

   two_way_if_null BOOLEAN default true,
   oneway TEXT default 'oneway',
   source TEXT default 'source',
   target TEXT default 'target')
  RETURNS TEXT AS
$BODY$


DECLARE
    edge_table TEXT := $1;
    s_in_rules TEXT[] := $2;
    s_out_rules TEXT[] := $3;
    t_in_rules TEXT[] := $4;
    t_out_rules TEXT[] := $5;
    rule TEXT;
    ecnt INTEGER;
    instr TEXT;
    naming record;
    sname TEXT;
    tname TEXT;
    tabname TEXT;
    vname TEXT;
    owname TEXT;
    sourcename TEXT;
    targetname TEXT;
    sourcetype TEXT;
    targettype TEXT;
    vertname TEXT;
    debuglevel TEXT;


BEGIN
  RAISE NOTICE 'PROCESSING:';
  RAISE NOTICE 'pgr_analyzeOneway(''%'',''%'',''%'',''%'',''%'',''%'',''%'',''%'',%)',
		edge_table, s_in_rules , s_out_rules, t_in_rules, t_out_rules, oneway, source ,target,two_way_if_null ;
  EXECUTE 'show client_min_messages' INTO debuglevel;

  BEGIN
    RAISE DEBUG 'Checking % exists',edge_table;
    EXECUTE 'SELECT * FROM _pgr_getTableName('||quote_literal(edge_table)||',2)' INTO naming;
    sname=naming.sname;
    tname=naming.tname;
    tabname=sname||'.'||tname;
    vname=tname||'_vertices_pgr';
    vertname= sname||'.'||vname;
    RAISE DEBUG '     --> OK';
    EXCEPTION WHEN raise_exception THEN
      RAISE NOTICE 'ERROR: something went wrong checking the table name';
      RETURN 'FAIL';
  END;

  BEGIN
       RAISE DEBUG 'Checking Vertices table';
       EXECUTE 'SELECT * FROM  _pgr_checkVertTab('||quote_literal(vertname) ||', ''{"id","ein","eout"}''::TEXT[])' INTO naming;
       EXECUTE 'UPDATE '||_pgr_quote_ident(vertname)||' SET eout=0 ,ein=0';
       RAISE DEBUG '     --> OK';
       EXCEPTION WHEN raise_exception THEN
          RAISE NOTICE 'ERROR: something went wrong checking the vertices table';
          RETURN 'FAIL';
  END;


  BEGIN
       RAISE DEBUG 'Checking column names in edge table';
       SELECT * INTO sourcename FROM _pgr_getColumnName(sname, tname,source,2);
       SELECT * INTO targetname FROM _pgr_getColumnName(sname, tname,target,2);
       SELECT * INTO owname FROM _pgr_getColumnName(sname, tname,oneway,2);


       perform _pgr_onError( sourcename IN (targetname,owname) or  targetname=owname, 2,
                       '_pgr_createToplogy',  'Two columns share the same name', 'Parameter names for oneway,source and target  must be different',
                       'Column names are OK');

       RAISE DEBUG '     --> OK';
       EXCEPTION WHEN raise_exception THEN
          RAISE NOTICE 'ERROR: something went wrong checking the column names';
          RETURN 'FAIL';
  END;

  BEGIN
       RAISE DEBUG 'Checking column types in edge table';
       SELECT * INTO sourcetype FROM _pgr_getColumnType(sname,tname,sourcename,1);
       SELECT * INTO targettype FROM _pgr_getColumnType(sname,tname,targetname,1);


       perform _pgr_onError(sourcetype NOT IN('integer','smallint','bigint') , 2,
                       '_pgr_createTopology',  'Wrong type of Column '|| sourcename, ' Expected type of '|| sourcename || ' is INTEGER,smallint OR BIGINT but '||sourcetype||' was found',
                       'Type of Column '|| sourcename || ' is ' || sourcetype);

       perform _pgr_onError(targettype NOT IN('integer','smallint','bigint') , 2,
                       '_pgr_createTopology',  'Wrong type of Column '|| targetname, ' Expected type of '|| targetname || ' is INTEGER,smallint OR BIGINTi but '||targettype||' was found',
                       'Type of Column '|| targetname || ' is ' || targettype);

       RAISE DEBUG '     --> OK';
       EXCEPTION WHEN raise_exception THEN
          RAISE NOTICE 'ERROR: something went wrong checking the column types';
          RETURN 'FAIL';
   END;



    RAISE NOTICE 'Analyzing graph for one way street errors.';

    rule := CASE WHEN two_way_if_null
            THEN owname || ' IS NULL OR '
            ELSE '' END;

    instr := '''' || array_to_string(s_in_rules, ''',''') || '''';
       EXECUTE 'UPDATE '||_pgr_quote_ident(vertname)||' a set ein=coalesce(ein,0)+b.cnt
      FROM (
         SELECT '|| sourcename ||', count(*) AS cnt
           FROM '|| tabname ||'
          WHERE '|| rule || owname ||' IN ('|| instr ||')
          GROUP BY '|| sourcename ||' ) b
     WHERE a.id=b.'|| sourcename;

    RAISE NOTICE 'Analysis 25%% complete ...';

    instr := '''' || array_to_string(t_in_rules, ''',''') || '''';
    EXECUTE 'UPDATE '||_pgr_quote_ident(vertname)||' a set ein=coalesce(ein,0)+b.cnt
        FROM (
         SELECT '|| targetname ||', count(*) AS cnt
           FROM '|| tabname ||'
          WHERE '|| rule || owname ||' IN ('|| instr ||')
          GROUP BY '|| targetname ||' ) b
        WHERE a.id=b.'|| targetname;

    RAISE NOTICE 'Analysis 50%% complete ...';

    instr := '''' || array_to_string(s_out_rules, ''',''') || '''';
    EXECUTE 'UPDATE '||_pgr_quote_ident(vertname)||' a set eout=coalesce(eout,0)+b.cnt
        FROM (
         SELECT '|| sourcename ||', count(*) AS cnt
           FROM '|| tabname ||'
          WHERE '|| rule || owname ||' IN ('|| instr ||')
          GROUP BY '|| sourcename ||' ) b
        WHERE a.id=b.'|| sourcename;
    RAISE NOTICE 'Analysis 75%% complete ...';

    instr := '''' || array_to_string(t_out_rules, ''',''') || '''';
    EXECUTE 'UPDATE '||_pgr_quote_ident(vertname)||' a set eout=coalesce(eout,0)+b.cnt
        FROM (
         SELECT '|| targetname ||', count(*) AS cnt
           FROM '|| tabname ||'
          WHERE '|| rule || owname ||' IN ('|| instr ||')
          GROUP BY '|| targetname ||' ) b
        WHERE a.id=b.'|| targetname;

    RAISE NOTICE 'Analysis 100%% complete ...';

    EXECUTE 'SELECT count(*)  FROM '||_pgr_quote_ident(vertname)||' WHERE ein=0 OR eout=0' INTO ecnt;

    RAISE NOTICE 'Found % potential problems in directionality' ,ecnt;

    RETURN 'OK';

END;
$BODY$
  LANGUAGE plpgsql VOLATILE STRICT;

-- COMMENTS

COMMENT ON FUNCTION pgr_analyzeOneWay(TEXT,TEXT[],TEXT[], TEXT[],TEXT[],BOOLEAN,TEXT,TEXT,TEXT)
IS 'pgr_analyzeOneWay
- Parameters
  - edge table
  - source in rules
  - source out rules,
  - target in rules
  - target out rules,
- Optional parameters
  - two_way_if_null := true
  - oneway := ''oneway'',
  - source := ''source''
  - target:=''target''
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_analyzeOneWay.html
';








---------------------------
-- pgr_createverticestable
---------------------------


--v2.6
CREATE FUNCTION pgr_createverticestable(
   TEXT,  -- edge table (required)
   the_geom TEXT DEFAULT 'the_geom'::TEXT,
   source TEXT DEFAULT 'source'::TEXT,
   target TEXT DEFAULT 'target'::TEXT,
    rows_where TEXT DEFAULT 'true'::TEXT
)
  RETURNS TEXT AS
$BODY$
DECLARE
    edge_table TEXT := $1;
    naming record;
    sridinfo record;
    sname TEXT;
    tname TEXT;
    tabname TEXT;
    vname TEXT;
    vertname TEXT;
    gname TEXT;
    sourcename TEXT;
    targetname TEXT;
    query TEXT;
    ecnt BIGINT;
    srid INTEGER;
    sourcetype TEXT;
    targettype TEXT;
    sql TEXT;
    totcount INTEGER;
    i INTEGER;
    notincluded INTEGER;
    included INTEGER;
    debuglevel TEXT;
    dummyRec record;
    fnName TEXT;
    err bool;


BEGIN
  fnName = 'pgr_createVerticesTable';
  RAISE NOTICE 'PROCESSING:';
  RAISE NOTICE 'pgr_createVerticesTable(''%'',''%'',''%'',''%'',''%'')',edge_table,the_geom,source,target,rows_where;
  EXECUTE 'show client_min_messages' INTO debuglevel;

  RAISE NOTICE 'Performing checks, please wait .....';

  RAISE DEBUG 'Checking % exists',edge_table;
        EXECUTE 'select * from _pgr_getTableName('|| quote_literal(edge_table)
                                                  || ',2,' || quote_literal(fnName) ||' )' INTO naming;

    sname=naming.sname;
    tname=naming.tname;
    tabname=sname||'.'||tname;
    vname=tname||'_vertices_pgr';
    vertname= sname||'.'||vname;
    rows_where = ' AND ('||rows_where||')';
  RAISE DEBUG '--> Edge table exists: OK';

  RAISE DEBUG 'Checking column names';
    select * INTO sourcename FROM _pgr_getColumnName(sname, tname,source,2, fnName);
    select * INTO targetname FROM _pgr_getColumnName(sname, tname,target,2, fnName);
    select * INTO gname      FROM _pgr_getColumnName(sname, tname,the_geom,2, fnName);


    err = sourcename IN (targetname,gname) OR  targetname=gname;
    perform _pgr_onError(err, 2, fnName,
        'Two columns share the same name', 'Parameter names for the_geom,source and target  must be different');
  RAISE DEBUG '--> Column names: OK';

  RAISE DEBUG 'Checking column types in edge table';
    select * INTO sourcetype FROM _pgr_getColumnType(sname,tname,sourcename,1, fnName);
    select * INTO targettype FROM _pgr_getColumnType(sname,tname,targetname,1, fnName);


    err = sourcetype not in('integer','smallint','bigint');
    perform _pgr_onError(err, 2, fnName,
        'Wrong type of Column source: '|| sourcename, ' Expected type of '|| sourcename || ' is integer, smallint or bigint but '||sourcetype||' was found');

    err = targettype not in('integer','smallint','bigint');
    perform _pgr_onError(err, 2, fnName,
        'Wrong type of Column target: '|| targetname, ' Expected type of '|| targetname || ' is integer, smallint or bigint but '||targettype||' was found');

  RAISE DEBUG '-->Column types:OK';

  RAISE DEBUG 'Checking SRID of geometry column';
     query= 'SELECT ST_SRID(' || quote_ident(gname) || ') as srid '
        || ' FROM ' || _pgr_quote_ident(tabname)
        || ' WHERE ' || quote_ident(gname)
        || ' IS NOT NULL LIMIT 1';
     RAISE DEBUG '%',query;
     EXECUTE query INTO sridinfo;

     err =  sridinfo IS NULL OR sridinfo.srid IS NULL;
     perform _pgr_onError(err, 2, fnName,
         'Can not determine the srid of the geometry '|| gname ||' in table '||tabname, 'Check the geometry of column '||gname);
     srid := sridinfo.srid;
  RAISE DEBUG '     --> OK';

  RAISE DEBUG 'Checking and creating Indices';
     perform _pgr_createIndex(sname, tname , sourcename , 'btree'::TEXT);
     perform _pgr_createIndex(sname, tname , targetname , 'btree'::TEXT);
     perform _pgr_createIndex(sname, tname , gname , 'gist'::TEXT);
  RAISE DEBUG '-->Check and create indices: OK';

     gname=quote_ident(gname);
     sourcename=quote_ident(sourcename);
     targetname=quote_ident(targetname);


  BEGIN
  RAISE DEBUG 'Checking Condition';
    -- issue #193 & issue #210 & #213
    -- this sql is for trying out the where clause
    -- the select * is to avoid any column name conflicts
    -- limit 1, just try on first record
    -- if the where clasuse is ill formed it will be caught in the exception
    sql = 'select * from '||_pgr_quote_ident(tabname)||' WHERE true'||rows_where ||' limit 1';
    EXECUTE sql INTO dummyRec;
    -- end

    -- if above where clasue works this one should work
    -- any error will be caught by the exception also
    sql = 'select count(*) from '||_pgr_quote_ident(tabname)||' WHERE (' || gname || ' IS NULL or '||
		sourcename||' is null or '||targetname||' is null)=true '||rows_where;
    RAISE DEBUG '%',sql;
    EXECUTE SQL  INTO notincluded;
    EXCEPTION WHEN OTHERS THEN
         RAISE NOTICE 'Got %', SQLERRM; -- issue 210,211
         RAISE NOTICE 'ERROR: Condition is not correct, please execute the following query to test your condition';
         RAISE NOTICE '%',sql;
         RETURN 'FAIL';
  END;




  BEGIN
     RAISE DEBUG 'initializing %',vertname;
       EXECUTE 'select * from _pgr_getTableName('||quote_literal(vertname)||',0)' INTO naming;
       IF sname=naming.sname  AND vname=naming.tname  THEN
           EXECUTE 'TRUNCATE TABLE '||_pgr_quote_ident(vertname)||' RESTART IDENTITY';
           EXECUTE 'SELECT DROPGEOMETRYCOLUMN('||quote_literal(sname)||','||quote_literal(vname)||','||quote_literal('the_geom')||')';
       ELSE
           set client_min_messages  to warning;
       	   EXECUTE 'CREATE TABLE '||_pgr_quote_ident(vertname)||' (id bigserial PRIMARY KEY,cnt INTEGER,chk INTEGER,ein INTEGER,eout INTEGER)';
       END IF;
       EXECUTE 'select addGeometryColumn('||quote_literal(sname)||','||quote_literal(vname)||','||
                quote_literal('the_geom')||','|| srid||', '||quote_literal('POINT')||', 2)';
       EXECUTE 'CREATE INDEX '||quote_ident(vname||'_the_geom_idx')||' ON '||_pgr_quote_ident(vertname)||'  USING GIST (the_geom)';
       EXECUTE 'set client_min_messages  to '|| debuglevel;
       RAISE DEBUG  '  ------>OK';
       EXCEPTION WHEN OTHERS THEN
         RAISE NOTICE 'Got %', SQLERRM; -- issue 210,211
         RAISE NOTICE 'ERROR: Initializing vertex table';
         RAISE NOTICE '%',sql;
         RETURN 'FAIL';
  END;

  BEGIN
       RAISE NOTICE 'Populating %, please wait...',vertname;
       sql= 'with
		lines as ((select distinct '||sourcename||' as id, _pgr_startpoint(st_linemerge('||gname||')) as the_geom from '||_pgr_quote_ident(tabname)||
		                  ' where ('|| gname || ' IS NULL
                                    OR '||sourcename||' is null
                                    OR '||targetname||' is null)=false
                                     '||rows_where||')
			UNION (select distinct '||targetname||' as id,_pgr_endpoint(st_linemerge('||gname||')) as the_geom from '||_pgr_quote_ident(tabname)||
			          ' where ('|| gname || ' IS NULL
                                    OR '||sourcename||' is null
                                    OR '||targetname||' is null)=false
                                     '||rows_where||'))
		,numberedLines as (select row_number() OVER (ORDER BY id) AS i,* from lines )
		,maxid as (select id,max(i) as maxi from numberedLines GROUP BY id)
		insert INTO '||_pgr_quote_ident(vertname)||'(id,the_geom)  (select id,the_geom  from numberedLines join maxid using(id) where i=maxi ORDER BY id)';
       RAISE DEBUG '%',sql;
       EXECUTE sql;
       GET DIAGNOSTICS totcount = ROW_COUNT;

       sql = 'select count(*) from '||_pgr_quote_ident(tabname)||' a, '||_pgr_quote_ident(vertname)||' b
            where '||sourcename||'=b.id AND '|| targetname||' IN (select id from '||_pgr_quote_ident(vertname)||')';
       RAISE DEBUG '%',sql;
       EXECUTE sql INTO included;



       EXECUTE 'select max(id) from '||_pgr_quote_ident(vertname) INTO ecnt;
       EXECUTE 'SELECT setval('||quote_literal(vertname||'_id_seq')||','||coalesce(ecnt,1)||' , false)';
       RAISE NOTICE '  ----->   VERTICES TABLE CREATED WITH  % VERTICES', totcount;
       RAISE NOTICE '                                       FOR   %  EDGES', included+notincluded;
       RAISE NOTICE '  Edges with NULL geometry,source or target: %',notincluded;
       RAISE NOTICE '                            Edges processed: %',included;
       RAISE NOTICE 'Vertices table for table % is: %',_pgr_quote_ident(tabname),_pgr_quote_ident(vertname);
       RAISE NOTICE '----------------------------------------------';
    END;

    RETURN 'OK';
 EXCEPTION WHEN OTHERS THEN
   RAISE NOTICE 'Unexpected error %', SQLERRM; -- issue 210,211
   RETURN 'FAIL';
END;
$BODY$
  LANGUAGE plpgsql VOLATILE STRICT;


-- COMMENTS


COMMENT ON FUNCTION pgr_createverticestable(TEXT, TEXT, TEXT, TEXT, TEXT)
IS 'pgr_createVerticesTable
- Parameters
  - Edge table name
- Optional parameters
  - the_geom := ''the_geom''
  - source := ''source''
  - target := ''target''
  - rows_where := ''true''
- Documentation:
  - https://docs.pgrouting.org/latest/en/pgr_createVerticesTable.html
';



---------------------------
-- pgr_nodeNetwork
---------------------------


--v2.6
CREATE FUNCTION pgr_nodeNetwork(
  TEXT, -- edge table (required)
  DOUBLE PRECISION, -- tolerance (required)
  id TEXT DEFAULT 'id',
  the_geom TEXT DEFAULT 'the_geom',
  table_ending TEXT DEFAULT 'noded',
  rows_where TEXT DEFAULT ''::TEXT,
  outall BOOLEAN DEFAULT FALSE)
RETURNS TEXT AS $BODY$
DECLARE
  
  edge_table TEXT := $1;
  tolerance TEXT := $2;
  p_num int := 0;
  p_ret TEXT := '';
  pgis_ver_old BOOLEAN := _pgr_versionless(postgis_lib_version(), '2.1.0.0');
  vst_line_substring TEXT;
  vst_line_locate_point TEXT;
  intab TEXT;
  outtab TEXT;
  n_pkey TEXT;
  n_geom TEXT;
  naming record;
  sname TEXT;
  tname TEXT;
  outname TEXT;
  srid INTEGER;
  sridinfo record;
  splits BIGINT;
  touched BIGINT;
  untouched BIGINT;
  geomtype TEXT;
  debuglevel TEXT;
  rows_where_out TEXT;
BEGIN
  RAISE NOTICE 'PROCESSING:';
  RAISE NOTICE 'id: %', id;
  RAISE NOTICE 'the_geom: %', the_geom;
  RAISE NOTICE 'table_ending: %', table_ending;
  RAISE NOTICE 'rows_where: %', rows_where;
  RAISE NOTICE 'outall: %', outall;
  RAISE NOTICE 'pgr_nodeNetwork(''%'', %, ''%'', ''%'', ''%'', ''%'',  %)',
    edge_table, tolerance, id,  the_geom, table_ending, rows_where, outall;
  RAISE NOTICE 'Performing checks, please wait .....';
  EXECUTE 'SHOW client_min_messages' INTO debuglevel;

  BEGIN
    RAISE DEBUG 'Checking % exists',edge_table;
    EXECUTE 'SELECT * FROM _pgr_getTableName('||quote_literal(edge_table)||',0)' INTO naming;
    sname=naming.sname;
    tname=naming.tname;
    IF sname IS NULL OR tname IS NULL THEN
    RAISE NOTICE '-------> % NOT found',edge_table;
      RETURN 'FAIL';
    ELSE
      RAISE DEBUG '  -----> OK';
    END IF;

    intab=sname||'.'||tname;
    outname=tname||'_'||table_ending;
    outtab= sname||'.'||outname;
    rows_where_out = CASE WHEN length(rows_where) > 2 AND NOT outall THEN ' AND (' || rows_where || ')' ELSE '' END;
    rows_where = CASE WHEN length(rows_where) > 2 THEN ' WHERE (' || rows_where || ')' ELSE '' END;
  END;

  BEGIN
    RAISE DEBUG 'Checking id column "%" columns IN  % ',id,intab;
    EXECUTE 'SELECT _pgr_getColumnName('||quote_literal(intab)||','||quote_literal(id)||')' INTO n_pkey;
    IF n_pkey is NULL THEN
      RAISE NOTICE  'ERROR: id column "%"  NOT found IN %',id,intab;
      RETURN 'FAIL';
    END IF;
  END;

  BEGIN
    RAISE DEBUG 'Checking id column "%" columns IN  % ',the_geom,intab;
    EXECUTE 'SELECT _pgr_getColumnName('||quote_literal(intab)||','||quote_literal(the_geom)||')' INTO n_geom;
    IF n_geom is NULL THEN
      RAISE NOTICE  'ERROR: the_geom  column "%"  NOT found IN %',the_geom,intab;
      RETURN 'FAIL';
    END IF;
  END;

  IF n_pkey=n_geom THEN
    RAISE NOTICE  'ERROR: id AND the_geom columns have the same name "%" IN %',n_pkey,intab;
    RETURN 'FAIL';
  END IF;

  BEGIN
    RAISE DEBUG 'Checking the SRID of the geometry "%"', n_geom;
    EXECUTE 'SELECT ST_SRID(' || quote_ident(n_geom) || ') AS srid '
        || ' FROM ' || _pgr_quote_ident(intab)
        || ' WHERE ' || quote_ident(n_geom)
        || ' IS NOT NULL LIMIT 1' INTO sridinfo;
    IF sridinfo IS NULL OR sridinfo.srid IS NULL THEN
      RAISE NOTICE 'ERROR: Can NOT determine the srid of the geometry "%" IN table %', n_geom,intab;
      RETURN 'FAIL';
    END IF;
    srid := sridinfo.srid;
    RAISE DEBUG '  -----> SRID found %',srid;
    EXCEPTION WHEN OTHERS THEN
      RAISE NOTICE 'ERROR: Can NOT determine the srid of the geometry "%" IN table %', n_geom,intab;
      RETURN 'FAIL';
  END;

  BEGIN
    RAISE DEBUG 'Checking "%" column IN % is indexed',n_pkey,intab;
    IF (_pgr_isColumnIndexed(intab,n_pkey)) THEN
      RAISE DEBUG '  ------>OK';
    ELSE
      RAISE DEBUG ' ------> Adding  index "%_%_idx".',n_pkey,intab;

      SET client_min_messages TO warning;
      EXECUTE 'CREATE INDEX '||tname||'_'||n_pkey||'_idx ON '||_pgr_quote_ident(intab)||' USING btree('||quote_ident(n_pkey)||')';
      EXECUTE 'SET client_min_messages TO '|| debuglevel;
    END IF;
  END;

  BEGIN
    RAISE DEBUG 'Checking "%" column IN % is indexed',n_geom,intab;
    IF (_pgr_iscolumnindexed(intab,n_geom)) THEN
      RAISE DEBUG '  ------>OK';
    ELSE
      RAISE DEBUG ' ------> Adding unique index "%_%_gidx".',intab,n_geom;
      SET client_min_messages TO warning;
      EXECUTE 'CREATE INDEX '
        || quote_ident(tname || '_' || n_geom || '_gidx' )
        || ' ON ' || _pgr_quote_ident(intab)
        || ' USING gist (' || quote_ident(n_geom) || ')';
      EXECUTE 'SET client_min_messages  TO '|| debuglevel;
    END IF;
  END;

---------------
  BEGIN
    RAISE DEBUG 'initializing %', outtab;
    EXECUTE 'SELECT * FROM _pgr_getTableName('||quote_literal(outtab)||',0)' INTO naming;
    IF sname=naming.sname  AND outname=naming.tname  THEN
      EXECUTE 'TRUNCATE TABLE '||_pgr_quote_ident(outtab)||' RESTART IDENTITY';
      EXECUTE 'SELECT DROPGEOMETRYCOLUMN('||quote_literal(sname)||','||quote_literal(outname)||','||quote_literal(n_geom)||')';
    ELSE
      SET client_min_messages TO warning;
      EXECUTE 'CREATE TABLE '||_pgr_quote_ident(outtab)||' (
        id bigserial PRIMARY KEY,
        old_id INTEGER,
        sub_id INTEGER,
        source BIGINT,
        target BIGINT)';
    END IF;
    EXECUTE 'SELECT geometrytype('||quote_ident(n_geom)||') FROM '||_pgr_quote_ident(intab)||' limit 1' INTO geomtype;
    IF geomtype IS NULL THEN
      RAISE NOTICE '-------> Table %.% must contain invalid geometries',sname, tname;
      RETURN 'FAIL';
    ELSE
      RAISE DEBUG '  ------> Create geometry column of type %', geomtype;
      EXECUTE 'SELECT addGeometryColumn('||quote_literal(sname)||','||quote_literal(outname)||','||quote_literal(n_geom)||','|| srid||', '||quote_literal(geomtype)||', 2)';
      EXECUTE 'CREATE INDEX '||quote_ident(outname||'_'||n_geom||'_idx')||' ON '||_pgr_quote_ident(outtab)||' USING GIST ('||quote_ident(n_geom)||')';
      EXECUTE 'SET client_min_messages TO '|| debuglevel;
      RAISE DEBUG '  ------>OK';
    END IF;
  END;
----------------

  RAISE NOTICE 'Processing, please wait .....';

  if pgis_ver_old THEN
    vst_line_substring    := 'st_line_substring';
    vst_line_locate_point := 'st_line_locate_point';
  ELSE
    vst_line_substring    := 'st_linesubstring';
    vst_line_locate_point := 'st_linelocatepoint';
  END IF;

  -- First creates temp table with intersection points
  p_ret = 'CREATE TEMP TABLE intergeom ON COMMIT DROP AS (
    SELECT l1.' || quote_ident(n_pkey) || ' AS l1id,
         l2.' || quote_ident(n_pkey) || ' AS l2id,
         l1.' || quote_ident(n_geom) || ' AS line,
         _pgr_startpoint(l2.' || quote_ident(n_geom) || ') AS source,
         _pgr_endpoint(l2.' || quote_ident(n_geom) || ') AS target,
         st_closestPoint(l1.' || quote_ident(n_geom) || ', l2.' || quote_ident(n_geom) || ') AS geom
    FROM (SELECT * FROM ' || _pgr_quote_ident(intab) || rows_where || ') AS l1
    JOIN (SELECT * FROM ' || _pgr_quote_ident(intab) || rows_where || ') AS l2
    ON (st_dwithin(l1.' || quote_ident(n_geom) || ', l2.' || quote_ident(n_geom) || ', ' || tolerance || '))'||
    'WHERE l1.' || quote_ident(n_pkey) || ' <> l2.' || quote_ident(n_pkey)||' AND
    st_equals(_pgr_startpoint(l1.' || quote_ident(n_geom) || '),_pgr_startpoint(l2.' || quote_ident(n_geom) || '))=false AND
    st_equals(_pgr_startpoint(l1.' || quote_ident(n_geom) || '),_pgr_endpoint(l2.' || quote_ident(n_geom) || '))=false AND
    st_equals(_pgr_endpoint(l1.' || quote_ident(n_geom) || '),_pgr_startpoint(l2.' || quote_ident(n_geom) || '))=false AND
    st_equals(_pgr_endpoint(l1.' || quote_ident(n_geom) || '),_pgr_endpoint(l2.' || quote_ident(n_geom) || '))=false )';
  RAISE DEBUG '%', p_ret;
  EXECUTE p_ret;

  -- second temp table with locus (index of intersection point on the line)
  -- to avoid updating the previous table
  -- we keep only intersection points occurring onto the line, not at one of its ends
  -- drop table if exists inter_loc;
  p_ret= 'CREATE TEMP TABLE inter_loc ON COMMIT DROP AS (
    SELECT l1id, l2id, ' || vst_line_locate_point || '(line,point) AS locus FROM (
    SELECT DISTINCT l1id, l2id, line, (ST_DumpPoints(geom)).geom AS point FROM intergeom) AS foo
    WHERE ' || vst_line_locate_point || '(line,point)<>0 and ' || vst_line_locate_point || '(line,point)<>1)';
  RAISE DEBUG '%',p_ret;
  EXECUTE p_ret;

  -- index on l1id
  CREATE INDEX inter_loc_id_idx ON inter_loc(l1id);

  -- Then computes the intersection on the lines subset, which is much smaller than full set
  -- as there are very few intersection points

  --- outab needs to be formally created with id, old_id, subid,the_geom, source,target
  --- so it can be inmediatly be used with createTopology

  P_RET = 'INSERT INTO '||_pgr_quote_ident(outtab)||' (old_id,sub_id,'||quote_ident(n_geom)||') (  WITH cut_locations AS
  (
    SELECT l1id AS lid, locus
    FROM inter_loc
    -- then generates start AND end locus for each line that have to be cut buy a location point
    UNION ALL
    SELECT DISTINCT i.l1id  AS lid, 0 AS locus
    FROM inter_loc i LEFT JOIN ' || _pgr_quote_ident(intab) || ' b ON (i.l1id = b.' || quote_ident(n_pkey) || ')
    UNION ALL
    SELECT DISTINCT i.l1id  AS lid, 1 AS locus
    FROM inter_loc i LEFT JOIN ' || _pgr_quote_ident(intab) || ' b ON (i.l1id = b.' || quote_ident(n_pkey) || ')
    ORDER BY lid, locus
  ),
  -- we generate a row_number index column for each input line
  -- to be able to self-join the table to cut a line between two consecutive locations
  loc_with_idx AS (
    SELECT lid, locus, row_number() OVER (PARTITION BY lid ORDER BY locus) AS idx
    FROM cut_locations
  )
  -- finally, each original line is cut with consecutive locations using linear referencing functions
  SELECT l.' || quote_ident(n_pkey) || ', loc1.idx AS sub_id, ' || vst_line_substring || '(l.' || quote_ident(n_geom) || ', loc1.locus, loc2.locus) AS ' || quote_ident(n_geom) || '
  FROM loc_with_idx loc1 JOIN loc_with_idx loc2 USING (lid) JOIN ' || _pgr_quote_ident(intab) || ' l ON (l.' || quote_ident(n_pkey) || ' = loc1.lid)
  WHERE loc2.idx = loc1.idx+1
    -- keeps only linestring geometries
    AND geometryType(' || vst_line_substring || '(l.' || quote_ident(n_geom) || ', loc1.locus, loc2.locus)) = ''LINESTRING'') ';
  RAISE DEBUG  '%',p_ret;
  EXECUTE p_ret;
  GET DIAGNOSTICS splits = ROW_COUNT;
    EXECUTE 'WITH diff AS (SELECT DISTINCT old_id FROM '||_pgr_quote_ident(outtab)||' )
         SELECT count(*) FROM diff' INTO touched;
  -- here, it misses all original line that did not need to be cut by intersection points: these lines
  -- are already clean
  -- inserts them in the final result: all lines which gid is not in the res table.
  EXECUTE 'INSERT INTO ' || _pgr_quote_ident(outtab) || ' (old_id , sub_id, ' || quote_ident(n_geom) || ')
    ( WITH used AS (SELECT DISTINCT old_id FROM '|| _pgr_quote_ident(outtab)||')
    SELECT ' ||  quote_ident(n_pkey) || ', 1 AS sub_id, ' ||  quote_ident(n_geom) ||
    ' FROM '|| _pgr_quote_ident(intab) ||' WHERE  '||quote_ident(n_pkey)||' NOT IN (SELECT * FROM used)' || rows_where_out || ')';
  GET DIAGNOSTICS untouched = ROW_COUNT;

  RAISE NOTICE '  Split Edges: %', touched;
  RAISE NOTICE ' Untouched Edges: %', untouched;
  RAISE NOTICE '     Total original Edges: %', touched+untouched;
    RAISE NOTICE ' Edges generated: %', splits;
  RAISE NOTICE ' Untouched Edges: %',untouched;
  RAISE NOTICE '       Total New segments: %', splits+untouched;
    RAISE NOTICE ' New Table: %', outtab;
    RAISE NOTICE '----------------------------------';

  DROP TABLE IF EXISTS intergeom;
  DROP TABLE IF EXISTS inter_loc;
  RETURN 'OK';
END;
$BODY$ LANGUAGE 'plpgsql' VOLATILE STRICT COST 100;

-- COMMENTS
COMMENT ON FUNCTION pgr_nodeNetwork(TEXT, DOUBLE PRECISION, TEXT, TEXT, TEXT, TEXT, BOOLEAN) IS 'pgr_nodeNetwork
- Parameters
  - Edge table name
  - tolerance
- Optional parameters
  - id := ''id''
  - the_geom := ''the_geom''
  - table_ending := ''noded''
  - rows_where := ''''
  - outall := false
- DOCUMENTATION:
  - https://docs.pgrouting.org/latest/en/pgr_nodeNetwork.html
';


--v3.0
CREATE FUNCTION pgr_extractVertices(
    TEXT,  -- SQL inner query (required)

    dryrun BOOLEAN DEFAULT false,

    OUT id BIGINT,
    OUT in_edges BIGINT[],
    OUT out_edges BIGINT[],
    OUT x FLOAT,
    OUT y FLOAT,
    OUT geom geometry
)
RETURNS SETOF RECORD AS
$BODY$
DECLARE
    edges_SQL TEXT;
    quoted TEXT;
    query TEXT;
    has_geom BOOLEAN := TRUE;
    has_source BOOLEAN := TRUE;
    has_points BOOLEAN := TRUE;
    has_id BOOLEAN := TRUE;
    rec RECORD;

BEGIN
  edges_sql := _pgr_checkQuery($1);
  has_id := _pgr_checkColumn(edges_sql, 'id', 'ANY-INTEGER', true, dryrun => $2);
  has_source := _pgr_checkColumn(edges_sql, 'source', 'ANY-INTEGER', true, dryrun => $2)
    AND _pgr_checkColumn(edges_sql, 'target', 'ANY-INTEGER', true, dryrun => $2);

  has_geom := _pgr_checkColumn(edges_sql, 'geom', 'geometry', true, dryrun => $2);
  has_points := _pgr_checkColumn(edges_sql, 'startpoint', 'geometry', true, dryrun => $2)
    AND _pgr_checkColumn(edges_sql, 'endpoint', 'geometry', true, dryrun => $2);

    IF has_geom AND has_id THEN
      -- SELECT id, geom
      query := $q$
        WITH

        main_sql AS (
          $q$ || edges_sql || $q$
        ),

        the_out AS (
          SELECT id::BIGINT AS out_edge, ST_StartPoint(geom) AS geom
          FROM main_sql
        ),

        agg_out AS (
          SELECT array_agg(out_edge ORDER BY out_edge) AS out_edges, ST_x(geom) AS x, ST_Y(geom) AS y, geom
          FROM the_out
          GROUP BY geom
        ),

        the_in AS (
          SELECT id::BIGINT AS in_edge, ST_EndPoint(geom) AS geom
          FROM main_sql
        ),

        agg_in AS (
          SELECT array_agg(in_edge ORDER BY in_edge) AS in_edges, ST_x(geom) AS x, ST_Y(geom) AS y, geom
          FROM the_in
          GROUP BY geom
        ),

        the_points AS (
          SELECT in_edges, out_edges, coalesce(agg_out.geom, agg_in.geom) AS geom
          FROM agg_out
          FULL OUTER JOIN agg_in USING (x, y)
        )

        SELECT row_number() over(ORDER BY ST_X(geom), ST_Y(geom)) AS id, in_edges, out_edges, ST_X(geom), ST_Y(geom), geom
        FROM the_points$q$;

    ELSIF has_geom AND NOT has_id THEN
      -- SELECT startpoint, endpoint
      -- can not get the ins and outs
      query := $q$
        WITH

        main_sql AS (
          $q$ || edges_sql || $q$
        ),

        sub_main AS (
          SELECT ST_StartPoint(geom) AS startpoint, ST_EndPoint(geom) AS endpoint
          FROM main_sql
        ),

        the_out AS (
          SELECT  DISTINCT ST_X(startpoint) AS x, ST_Y(startpoint) AS y, startpoint AS geom
          FROM sub_main
        ),

        the_in AS (
            SELECT DISTINCT ST_X(endpoint) AS x, ST_Y(endpoint) AS y, endpoint AS geom
          FROM sub_main
        ),

        the_points AS (
          SELECT x, y, coalesce(the_out.geom, the_in.geom) AS geom
          FROM the_out
          FULL OUTER JOIN the_in USING (x, y)
        )

        SELECT row_number() over(ORDER BY  ST_X(geom), ST_Y(geom)) AS id, NULL::BIGINT[], NULL::BIGINT[], x, y, geom
        FROM the_points$q$;

    ELSIF has_points AND has_id THEN
      -- SELECT id, startpoint, endpoint
      query := $q$
        WITH

        main_sql AS (
          $q$ || edges_sql || $q$
        ),

        the_out AS (
          SELECT id::BIGINT AS out_edge, startpoint AS geom
          FROM main_sql
        ),

        agg_out AS (
          SELECT array_agg(out_edge ORDER BY out_edge) AS out_edges, ST_x(geom) AS x, ST_Y(geom) AS y, geom
          FROM the_out
          GROUP BY geom
        ),

        the_in AS (
          SELECT id::BIGINT AS in_edge, endpoint AS geom
          FROM main_sql
        ),

        agg_in AS (
          SELECT array_agg(in_edge ORDER BY in_edge) AS in_edges, ST_x(geom) AS x, ST_Y(geom) AS y, geom
          FROM the_in
          GROUP BY geom
        ),

        the_points AS (
          SELECT in_edges, out_edges, coalesce(agg_out.geom, agg_in.geom) AS geom
          FROM agg_out
          FULL OUTER JOIN agg_in USING (x, y)
        )

        SELECT row_number() over(ORDER BY  ST_X(geom), ST_Y(geom)) AS id, in_edges, out_edges, ST_X(geom), ST_Y(geom), geom
        FROM the_points$q$;

    ELSIF has_points AND NOT has_id THEN
      -- SELECT startpoint, endpoint
      -- can not get the ins and outs
      query := $q$
        WITH

        main_sql AS (
          $q$ || edges_sql || $q$
        ),

        the_out AS (
          SELECT DISTINCT ST_X(startpoint) AS x, ST_Y(startpoint) AS y, startpoint AS geom
          FROM main_sql
        ),

        the_in AS (
            SELECT DISTINCT ST_X(endpoint) AS x, ST_Y(endpoint) AS y, endpoint AS geom
          FROM main_sql
        ),

        the_points AS (
          SELECT x, y, coalesce(the_out.geom, the_in.geom) AS geom
          FROM the_out
          FULL OUTER JOIN the_in USING (x, y)
        )

        SELECT row_number() over(ORDER BY  ST_X(geom), ST_Y(geom)) AS id, NULL::BIGINT[], NULL::BIGINT[], x, y, geom
        FROM the_points$q$;

    ELSIF has_source AND has_id THEN
      -- SELECT id, source, target
      query := $q$ WITH

        main_sql AS (
          $q$ || edges_sql || $q$
        ),

        agg_out AS (
          SELECT source AS vid, array_agg(id::BIGINT) AS out_edges
          FROM main_sql
          GROUP BY source
        ),

        agg_in AS (
          SELECT target AS vid, array_agg(id::BIGINT) AS in_edges
          FROM main_sql
          GROUP BY target
        ),

        the_points AS (
          SELECT vid, in_edges, out_edges
          FROM agg_out
          FULL OUTER JOIN agg_in USING (vid)
        )

        SELECT vid::BIGINT AS id, in_edges, out_edges, NULL::FLOAT, NULL::FLOAT, NULL::geometry
        FROM the_points$q$;


    ELSIF has_source AND NOT has_id THEN
      -- SELECT source, target
      query := $q$
        WITH

        main_sql AS (
          $q$ || edges_sql || $q$
        ),


        the_points AS (
          SELECT source AS vid FROM main_sql
          UNION
          SELECT target FROM main_sql
        )

        SELECT DISTINCT vid::BIGINT AS id, NULL::BIGINT[], NULL::BIGINT[], NULL::FLOAT, NULL::FLOAT, NULL::geometry
        FROM the_points$q$;

    ELSE
        RAISE EXCEPTION 'Missing column'
        USING HINT = 'Please check query: '|| $1;

    END IF;

    IF dryrun THEN
      RAISE NOTICE '%', query || ';';
    ELSE
      RETURN QUERY EXECUTE query;
    END IF;

    EXCEPTION WHEN OTHERS THEN
        RAISE EXCEPTION '%', SQLERRM
        USING HINT = 'Please check query: '|| $1;

END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;


-- COMMENTS


COMMENT ON FUNCTION pgr_extractVertices(TEXT, BOOLEAN)
IS 'pgr_extractVertices
- PROPOSED
- Parameters
  - Edges SQL with columns: [id,] startpoint, endpoint
        OR
  - Edges SQL with columns: [id,] source, target
        OR
  - Edges SQL with columns: [id,] geom
- Documentation:
- https://docs.pgrouting.org/latest/en/pgr_extractVertices.html
';



--v3.4
CREATE FUNCTION pgr_degree(
  TEXT,  -- Edges SQL
  TEXT,  -- Vertices SQL

  dryrun BOOLEAN DEFAULT false,

  OUT node BIGINT,
  OUT degree BIGINT
)
RETURNS SETOF RECORD AS
$BODY$
DECLARE
  edges_sql TEXT;
  vertices_sql TEXT;
  has_in_edges BOOLEAN := TRUE;
  has_out_edges BOOLEAN := TRUE;
  eids TEXT;
  query TEXT;

BEGIN

  edges_sql := _pgr_checkQuery($1);
  PERFORM _pgr_checkColumn(edges_sql, 'id', 'ANY-INTEGER', dryrun => $3);

  vertices_sql := _pgr_checkQuery($2);
  PERFORM _pgr_checkColumn(vertices_sql, 'id', 'ANY-INTEGER', dryrun => $3);
  has_in_edges := _pgr_checkColumn(vertices_sql, 'in_edges', 'ANY-INTEGER[]', true, dryrun => $3);
  has_out_edges := _pgr_checkColumn(vertices_sql, 'out_edges', 'ANY-INTEGER[]', true, dryrun => $3);

  IF has_in_edges THEN
    eids = $$coalesce(in_edges::BIGINT[], '{}'::BIGINT[])$$;
  END IF;

  IF has_out_edges THEN
    IF has_in_edges THEN
      eids = E'\n          ' || eids
            || E'\n          ||\n          '
            || $$coalesce(out_edges::BIGINT[], '{}'::BIGINT[])$$;
    ELSE
      eids = $$coalesce(out_edges::BIGINT[], '{}'::BIGINT[])$$;
    END IF;
  ELSE
    IF NOT has_in_edges THEN
      RAISE EXCEPTION 'Missing column'
      USING HINT = 'Column "in_edges" and/or "out_edges" is missing in'||E'\n'||vertices_sql;
    END IF;
  END IF;

  query := format($q$
    WITH

    -- a sub set of edges of the graph goes here
    g_edges AS (
      $q$ || edges_sql || $q$
    ),

    -- sub set of vertices of the graph goes here
    all_vertices AS (
      $q$ || vertices_sql || $q$
    ),

    g_vertices AS (
      SELECT id,
        unnest(%s) AS eid
      FROM all_vertices
    ),

    totals AS (
      SELECT v.id, count(*)
      FROM g_vertices AS v
      JOIN g_edges AS e ON (e.id = eid) GROUP BY v.id
    )

    SELECT id::BIGINT, coalesce(count, 0)::BIGINT FROM all_vertices LEFT JOIN totals USING (id)
    $q$, eids);

  IF dryrun THEN
    RAISE NOTICE '%', query || ';';
  ELSE
    RETURN QUERY EXECUTE query;
  END IF;

  EXCEPTION WHEN OTHERS THEN
    RAISE EXCEPTION '%', SQLERRM
    USING HINT = 'Please check query: '|| $1;

END;
$BODY$
LANGUAGE plpgsql VOLATILE STRICT;


-- COMMENTS
COMMENT ON FUNCTION pgr_degree(TEXT, TEXT, BOOLEAN)
IS 'pgr_degree
- PROPOSED
- Parameters
- Edges SQL with columns: id
- Vertices SQL with columns: id, in_edges, out_edges
- Documentation:
- https://docs.pgrouting.org/latest/en/pgr_degree.html
';


--v3.4
CREATE FUNCTION pgr_findCloseEdges(
  TEXT,
  geometry,
  FLOAT,
  cap INTEGER DEFAULT 1,
  partial BOOLEAN DEFAULT true,
  dryrun BOOLEAN DEFAULT false,

  OUT edge_id BIGINT,
  OUT fraction FLOAT,
  OUT side CHAR,
  OUT distance FLOAT,
  OUT geom geometry,
  OUT edge geometry)
returns SETOF RECORD AS
$BODY$
DECLARE
  edges_SQL TEXT;
  has_id BOOLEAN;
  has_geom BOOLEAN;
  ret_query TEXT;
  ret_query_end TEXT;
BEGIN

  IF ($3 < 0) THEN
    RAISE EXCEPTION 'Invalid value for tolerance';
  END IF;

  IF (cap <= 0) THEN
    RAISE EXCEPTION 'Invalid value for cap';
  END IF;

  edges_sql := _pgr_checkQuery($1);
  has_id := _pgr_checkColumn(edges_sql, 'id', 'ANY-INTEGER', false, dryrun => dryrun);
  has_geom := _pgr_checkColumn(edges_sql, 'geom', 'geometry', false, dryrun => dryrun);


  ret_query = format(
    $q$
    WITH
    edges_sql AS (%1$s),
    point_sql AS (SELECT %2$L::geometry AS point)

    SELECT
      id::BIGINT AS edge_id,
      ST_LineLocatePoint(geom, point) AS fraction,
      CASE WHEN ST_Intersects(ST_Buffer(geom, %3$s, 'side=right endcap=flat'), point)
           THEN 'r'
           ELSE 'l' END::CHAR AS side,
    $q$, edges_sql, $2, $3);

  ret_query_end = format(
    $q$
    FROM  edges_sql, point_sql
    WHERE ST_DWithin(geom,  point, %1$s)
    ORDER BY geom <-> point LIMIT %2$s
    $q$, $3, cap);

  IF partial AND cap = 1 AND NOT dryrun THEN
    

    ret_query =
      ret_query
      || $q$
        NULL::FLOAT,
        NULL::geometry,
        NULL::geometry
        $q$
      || ret_query_end;

  ELSIF partial AND NOT dryrun THEN
    

    ret_query =
      ret_query
      || $q$
        geom <-> point AS distance,
        NULL::geometry,
        NULL::geometry
        $q$
      || ret_query_end;

  ELSE
    

    ret_query =
      ret_query
      || $q$
        geom <-> point AS distance,
        ST_ClosestPoint(geom, point) AS new_point,
        ST_MakeLine(point, ST_ClosestPoint(geom, point)) AS new_line
        $q$
      || ret_query_end;

  END IF;

  IF dryrun THEN
    RAISE NOTICE '%', ret_query;
    RETURN;
  END IF;

  RETURN query EXECUTE ret_query;

END;
$BODY$
LANGUAGE PLPGSQL VOLATILE STRICT
COST 5;

--v3.4
CREATE FUNCTION pgr_findCloseEdges(
  TEXT,
  geometry[],
  FLOAT,
  cap INTEGER DEFAULT 1,
  partial BOOLEAN DEFAULT true,
  dryrun BOOLEAN DEFAULT false,

  OUT edge_id BIGINT,
  OUT fraction FLOAT,
  OUT side CHAR,
  OUT distance FLOAT,
  OUT geom geometry,
  OUT edge geometry)
returns SETOF RECORD AS
$BODY$
DECLARE
  edges_SQL TEXT;
  has_id BOOLEAN;
  has_geom BOOLEAN;
  ret_query TEXT;
  ret_query_end TEXT;
BEGIN

  IF ($3 < 0) THEN
    RAISE EXCEPTION 'Invalid value for tolerance';
  END IF;

  edges_sql := _pgr_checkQuery($1);
  has_id := _pgr_checkColumn(edges_sql, 'id', 'ANY-INTEGER', false, dryrun => dryrun);
  has_geom := _pgr_checkColumn(edges_sql, 'geom', 'geometry', false, dryrun => dryrun);

  ret_query = format(
    $q$
WITH
edges_sql AS (%1$s),
point_sql AS (SELECT unnest(%2$L::geometry[]) AS point),
results AS (
  SELECT
    id::BIGINT AS edge_id,
    ST_LineLocatePoint(geom, point) AS fraction,
    CASE WHEN ST_Intersects(ST_Buffer(geom, %3$s, 'side=right endcap=flat'), point)
         THEN 'r'
         ELSE 'l' END::CHAR AS side,
    geom <-> point AS distance,
    point,
    $q$, edges_sql, $2, $3);

  ret_query_end = format(
    $q$
  FROM  edges_sql, point_sql
  WHERE ST_DWithin(geom, point, %1$s)
  ORDER BY geom <-> point),
prepare_cap AS (
  SELECT row_number() OVER (PARTITION BY point ORDER BY point, distance) AS rn, *
  FROM results)
SELECT edge_id, fraction, side, distance, point, new_line
FROM prepare_cap
WHERE rn <= %2$s
    $q$, $3, cap);

  IF partial AND NOT dryrun THEN

    ret_query = ret_query
      || $q$NULL::geometry AS new_line$q$
      || ret_query_end;

  ELSE

    ret_query = ret_query
      || $q$ST_MakeLine(point, ST_ClosestPoint(geom, point)) AS new_line $q$
      || ret_query_end;

  END IF;

  IF dryrun THEN
    RAISE NOTICE '%', ret_query;
    RETURN;
  END IF;

  RETURN query EXECUTE ret_query;

END;
$BODY$
LANGUAGE PLPGSQL VOLATILE STRICT
COST 5;

-- COMMENTS
COMMENT ON FUNCTION pgr_findCloseEdges(TEXT, GEOMETRY, FLOAT, INTEGER, BOOLEAN, BOOLEAN)
IS 'pgr_findCloseEdges(One Point)
- Parameters:
  - Edges SQL with columns: id, geom
  - POINT geometry
  - Maximum separation between geometries
- Optional Parameters
  - cap => 1: at most one answer
  - partial => true: do minimal calculations
  - dryrun => false: do not output code
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_findCloseEdges.html
';

COMMENT ON FUNCTION pgr_findCloseEdges(TEXT, GEOMETRY, FLOAT, INTEGER, BOOLEAN, BOOLEAN)
IS 'pgr_findCloseEdges(Many Points)
- Parameters:
  - Edges SQL with columns: id, geom
  - Array of POINT geometries
  - Maximum separation between geometries
- Optional Parameters
  - cap => 1: at most one answer
  - partial => true: do minimal calculations
  - dryrun => false: do not output code
- Documentation:
   - https://docs.pgrouting.org/latest/en/pgr_findCloseEdges.html
';