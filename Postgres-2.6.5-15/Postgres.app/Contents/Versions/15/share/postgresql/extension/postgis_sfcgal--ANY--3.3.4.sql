\echo Use "CREATE EXTENSION postgis_sfcgal" to load this file. \quit
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
--
----
-- PostGIS - Spatial Types for PostgreSQL
-- http://postgis.net
--
-- Copyright (C) 2011 Regina Obe <lr@pcorp.us>
--
-- This is free software; you can redistribute and/or modify it under
-- the terms of the GNU General Public Licence. See the COPYING file.
--
-- Author: Regina Obe <lr@pcorp.us>
--
-- This is a suite of SQL helper functions for use during a PostGIS extension install/upgrade
-- The functions get uninstalled after the extention install/upgrade process
---------------------------
-- postgis_extension_remove_objects: This function removes objects of a particular class from an extension
-- this is needed because there is no ALTER EXTENSION DROP FUNCTION/AGGREGATE command
-- and we can't CREATE OR REPALCe functions whose signatures have changed and we can drop them if they are part of an extention
-- So we use this to remove it from extension first before we drop
CREATE FUNCTION postgis_extension_remove_objects(param_extension text, param_type text)
  RETURNS boolean AS
$$
DECLARE
	var_sql text := '';
	var_r record;
	var_result boolean := false;
	var_class text := '';
	var_is_aggregate boolean := false;
	var_sql_list text := '';
	var_pgsql_version integer := pg_catalog.current_setting('server_version_num');
BEGIN
		var_class := CASE WHEN pg_catalog.lower(param_type) OPERATOR(pg_catalog.=)'function' OR pg_catalog.lower(param_type) OPERATOR(pg_catalog.=) 'aggregate' THEN 'pg_catalog.pg_proc' ELSE '' END;
		var_is_aggregate := CASE WHEN pg_catalog.lower(param_type) OPERATOR(pg_catalog.=) 'aggregate' THEN true ELSE false END;

		IF var_pgsql_version OPERATOR(pg_catalog.<) 110000 THEN
			var_sql_list := $sql$SELECT 'ALTER EXTENSION ' OPERATOR(pg_catalog.||)  e.extname OPERATOR(pg_catalog.||) ' DROP ' OPERATOR(pg_catalog.||) $3 OPERATOR(pg_catalog.||) ' ' OPERATOR(pg_catalog.||) COALESCE(proc.proname OPERATOR(pg_catalog.||) '(' OPERATOR(pg_catalog.||) oidvectortypes(proc.proargtypes) OPERATOR(pg_catalog.||) ')' ,typ.typname, cd.relname, op.oprname,
					cs.typname OPERATOR(pg_catalog.||) ' AS ' OPERATOR(pg_catalog.||) ct.typname OPERATOR(pg_catalog.||) ') ', opcname, opfname) OPERATOR(pg_catalog.||) ';' AS remove_command
			FROM pg_catalog.pg_depend As d INNER JOIN pg_catalog.pg_extension As e
				ON d.refobjid OPERATOR(pg_catalog.=) e.oid INNER JOIN pg_catalog.pg_class As c ON
					c.oid OPERATOR(pg_catalog.=) d.classid
					LEFT JOIN pg_catalog.pg_proc AS proc ON proc.oid OPERATOR(pg_catalog.=) d.objid
					LEFT JOIN pg_catalog.pg_type AS typ ON typ.oid OPERATOR(pg_catalog.=) d.objid
					LEFT JOIN pg_catalog.pg_class As cd ON cd.oid OPERATOR(pg_catalog.=) d.objid
					LEFT JOIN pg_operator As op ON op.oid OPERATOR(pg_catalog.=) d.objid
					LEFT JOIN pg_catalog.pg_cast AS ca ON ca.oid OPERATOR(pg_catalog.=) d.objid
					LEFT JOIN pg_catalog.pg_type AS cs ON ca.castsource OPERATOR(pg_catalog.=) cs.oid
					LEFT JOIN pg_catalog.pg_type AS ct ON ca.casttarget OPERATOR(pg_catalog.=) ct.oid
					LEFT JOIN pg_opclass As oc ON oc.oid OPERATOR(pg_catalog.=) d.objid
					LEFT JOIN pg_opfamily As ofa ON ofa.oid OPERATOR(pg_catalog.=) d.objid
			WHERE d.deptype OPERATOR(pg_catalog.=) 'e' and e.extname OPERATOR(pg_catalog.=) $1 and c.relname OPERATOR(pg_catalog.=) $2 AND COALESCE(proc.proisagg, false) OPERATOR(pg_catalog.=) $4;$sql$;
		ELSE -- for PostgreSQL 11 and above, they removed proc.proisagg among others and replaced with some func type thing
			var_sql_list := $sql$SELECT 'ALTER EXTENSION ' OPERATOR(pg_catalog.||) e.extname OPERATOR(pg_catalog.||) ' DROP ' OPERATOR(pg_catalog.||) $3 OPERATOR(pg_catalog.||) ' ' OPERATOR(pg_catalog.||) COALESCE(proc.proname OPERATOR(pg_catalog.||) '(' OPERATOR(pg_catalog.||) oidvectortypes(proc.proargtypes) OPERATOR(pg_catalog.||) ')' ,typ.typname, cd.relname, op.oprname,
					cs.typname OPERATOR(pg_catalog.||) ' AS ' OPERATOR(pg_catalog.||) ct.typname OPERATOR(pg_catalog.||) ') ', opcname, opfname) OPERATOR(pg_catalog.||) ';' AS remove_command
			FROM pg_catalog.pg_depend As d INNER JOIN pg_catalog.pg_extension As e
				ON d.refobjid OPERATOR(pg_catalog.=) e.oid INNER JOIN pg_catalog.pg_class As c ON
					c.oid OPERATOR(pg_catalog.=) d.classid
					LEFT JOIN pg_catalog.pg_proc AS proc ON proc.oid OPERATOR(pg_catalog.=) d.objid
					LEFT JOIN pg_catalog.pg_type AS typ ON typ.oid OPERATOR(pg_catalog.=) d.objid
					LEFT JOIN pg_catalog.pg_class As cd ON cd.oid OPERATOR(pg_catalog.=) d.objid
					LEFT JOIN pg_operator As op ON op.oid OPERATOR(pg_catalog.=) d.objid
					LEFT JOIN pg_catalog.pg_cast AS ca ON ca.oid OPERATOR(pg_catalog.=) d.objid
					LEFT JOIN pg_catalog.pg_type AS cs ON ca.castsource OPERATOR(pg_catalog.=) cs.oid
					LEFT JOIN pg_catalog.pg_type AS ct ON ca.casttarget OPERATOR(pg_catalog.=) ct.oid
					LEFT JOIN pg_opclass As oc ON oc.oid OPERATOR(pg_catalog.=) d.objid
					LEFT JOIN pg_opfamily As ofa ON ofa.oid OPERATOR(pg_catalog.=) d.objid
			WHERE d.deptype OPERATOR(pg_catalog.=) 'e' and e.extname OPERATOR(pg_catalog.=) $1 and c.relname OPERATOR(pg_catalog.=) $2 AND (proc.prokind OPERATOR(pg_catalog.=) 'a')  OPERATOR(pg_catalog.=) $4;$sql$;
		END IF;

		FOR var_r IN EXECUTE var_sql_list  USING param_extension, var_class, param_type, var_is_aggregate
		LOOP
			var_sql := var_sql OPERATOR(pg_catalog.||) var_r.remove_command OPERATOR(pg_catalog.||) ';';
		END LOOP;
		IF var_sql > '' THEN
			EXECUTE var_sql;
			var_result := true;
		END IF;

		RETURN var_result;
END;
$$
LANGUAGE plpgsql VOLATILE;

CREATE FUNCTION postgis_extension_drop_if_exists(param_extension text, param_statement text)
  RETURNS boolean AS
$$
DECLARE
	var_sql_ext text := 'ALTER EXTENSION ' OPERATOR(pg_catalog.||) pg_catalog.quote_ident(param_extension) OPERATOR(pg_catalog.||) ' ' OPERATOR(pg_catalog.||) pg_catalog.replace(param_statement, 'IF EXISTS', '');
	var_result boolean := false;
BEGIN
	BEGIN
		EXECUTE var_sql_ext;
		var_result := true;
	EXCEPTION
		WHEN OTHERS THEN
			--this is to allow ignoring if the object does not exist in extension
			var_result := false;
	END;
	RETURN var_result;
END;
$$
LANGUAGE plpgsql VOLATILE;

CREATE FUNCTION postgis_extension_AddToSearchPath(a_schema_name text)
RETURNS text
AS
$$
DECLARE
	var_result text;
	var_cur_search_path text;
BEGIN

	WITH settings AS (
		SELECT pg_catalog.unnest(setconfig) config
		FROM pg_catalog.pg_db_role_setting
		WHERE setdatabase OPERATOR(pg_catalog.=) (
			SELECT oid
			FROM pg_catalog.pg_database
			WHERE datname OPERATOR(pg_catalog.=) pg_catalog.current_database()
		) and setrole OPERATOR(pg_catalog.=) 0
	)
	SELECT pg_catalog.regexp_replace(config, '^search_path=', '')
	FROM settings WHERE config like 'search_path=%'
	INTO var_cur_search_path;

	RAISE NOTICE 'cur_search_path from pg_db_role_setting is %', var_cur_search_path;

	IF var_cur_search_path IS NULL THEN
		SELECT reset_val
		INTO var_cur_search_path
		FROM pg_catalog.pg_settings
		WHERE name OPERATOR(pg_catalog.=) 'search_path';

		RAISE NOTICE 'cur_search_path from pg_settings is %', var_cur_search_path;
	END IF;


	IF var_cur_search_path LIKE '%' OPERATOR(pg_catalog.||) pg_catalog.quote_ident(a_schema_name) OPERATOR(pg_catalog.||) '%' THEN
		var_result := a_schema_name OPERATOR(pg_catalog.||) ' already in database search_path';
	ELSE
		var_cur_search_path := var_cur_search_path OPERATOR(pg_catalog.||) ', '
                       OPERATOR(pg_catalog.||) pg_catalog.quote_ident(a_schema_name);
		EXECUTE 'ALTER DATABASE ' OPERATOR(pg_catalog.||) pg_catalog.quote_ident(pg_catalog.current_database())
                             OPERATOR(pg_catalog.||) ' SET search_path = ' OPERATOR(pg_catalog.||) var_cur_search_path;
		var_result := a_schema_name OPERATOR(pg_catalog.||) ' has been added to end of database search_path ';
	END IF;

	EXECUTE 'SET search_path = ' OPERATOR(pg_catalog.||) var_cur_search_path;

  RETURN var_result;
END
$$
LANGUAGE 'plpgsql' VOLATILE STRICT;

-- PostGIS - Spatial Types for PostgreSQL
-- http://postgis.net
--
-- Copyright (C) 2020 Regina Obe <lr@pcorp.us>
-- This is free software; you can redistribute and/or modify it under
-- the terms of the GNU General Public Licence. See the COPYING file.
--
--
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
-- These are functions that need to be dropped beforehand
-- where the argument names may have changed  --
-- so have to be dropped before upgrade can happen --
-- argument names changed --


--
-- UPGRADE SCRIPT TO PostGIS 3.3.4
--

LOAD '$libdir/postgis_sfcgal-3';

DO $$
DECLARE
    old_scripts text;
    new_scripts text;
    old_ver_int int[];
    new_ver_int int[];
    old_maj text;
    new_maj text;
    postgis_upgrade_info RECORD;
    postgis_upgrade_info_func_code TEXT;
BEGIN

    old_scripts := postgis_scripts_installed();
    new_scripts := '3.3.4';

    BEGIN
        new_ver_int := pg_catalog.string_to_array(
            pg_catalog.regexp_replace(
                new_scripts,
                '[^\d.].*',
                ''
            ),
            '.'
        )::int[];
    EXCEPTION WHEN OTHERS THEN
        RAISE EXCEPTION 'Cannot parse new version % into integers', new_scripts;
    END;

    BEGIN
        old_ver_int := pg_catalog.string_to_array(
            pg_catalog.regexp_replace(
                old_scripts,
                '[^\d.].*',
                ''
            ),
            '.'
        )::int[];
    EXCEPTION WHEN OTHERS THEN
        RAISE EXCEPTION 'Cannot parse old version % into integers', old_scripts;
    END;

    -- Guard against downgrade
    IF new_ver_int < old_ver_int
    THEN
        RAISE EXCEPTION 'Downgrade of postgis from version % to version % is forbidden', old_scripts, new_scripts;
    END IF;


    -- Check for hard-upgrade being required
    SELECT into old_maj pg_catalog.substring(old_scripts, 1, 1);
    SELECT into new_maj pg_catalog.substring(new_scripts, 1, 1);

    -- 2.x to 3.x was upgrade-compatible, see
    -- https://trac.osgeo.org/postgis/ticket/4170#comment:1
    IF new_maj = '3' AND old_maj = '2' THEN
        old_maj = '3'; -- let's pretend old major = new major
    END IF;

    IF old_maj != new_maj THEN
        RAISE EXCEPTION 'Upgrade of postgis from version % to version % requires a dump/reload. See PostGIS manual for instructions', old_scripts, new_scripts;
    END IF;

    WITH versions AS (
      SELECT '3.3.4'::text as upgraded,
      postgis_scripts_installed() as installed
    ) SELECT
      upgraded as scripts_upgraded,
      installed as scripts_installed,
      pg_catalog.substring(upgraded, '([0-9]+)\.')::int * 100 +
      pg_catalog.substring(upgraded, '[0-9]+\.([0-9]+)(\.|$)')::int
        as version_to_num,
      pg_catalog.substring(installed, '([0-9]+)\.')::int * 100 +
      pg_catalog.substring(installed, '[0-9]+\.([0-9]+)(\.|$)')::int
        as version_from_num,
      installed ~ 'dev|alpha|beta'
        as version_from_isdev
      FROM versions INTO postgis_upgrade_info
    ;

    postgis_upgrade_info_func_code := pg_catalog.format($func_code$
        CREATE FUNCTION _postgis_upgrade_info(OUT scripts_upgraded TEXT,
                                              OUT scripts_installed TEXT,
                                              OUT version_to_num INT,
                                              OUT version_from_num INT,
                                              OUT version_from_isdev BOOLEAN)
        AS
        $postgis_upgrade_info$
        BEGIN
            scripts_upgraded := %L :: TEXT;
            scripts_installed := %L :: TEXT;
            version_to_num := %L :: INT;
            version_from_num := %L :: INT;
            version_from_isdev := %L :: BOOLEAN;
            RETURN;
        END
        $postgis_upgrade_info$ LANGUAGE 'plpgsql' IMMUTABLE;
        $func_code$,
        postgis_upgrade_info.scripts_upgraded,
        postgis_upgrade_info.scripts_installed,
        postgis_upgrade_info.version_to_num,
        postgis_upgrade_info.version_from_num,
        postgis_upgrade_info.version_from_isdev);
    RAISE DEBUG 'Creating function %', postgis_upgrade_info_func_code;
    EXECUTE postgis_upgrade_info_func_code;
END
$$
LANGUAGE 'plpgsql';

CREATE OR REPLACE FUNCTION postgis_sfcgal_scripts_installed() RETURNS text
	AS $$ SELECT trim('3.3.4'::text || $rev$ 3.3.4 $rev$) AS version $$
	LANGUAGE 'sql' IMMUTABLE;
CREATE OR REPLACE FUNCTION postgis_sfcgal_version() RETURNS text
        AS '$libdir/postgis_sfcgal-3'
        LANGUAGE 'c' IMMUTABLE;
CREATE OR REPLACE FUNCTION postgis_sfcgal_full_version() RETURNS text
        AS '$libdir/postgis_sfcgal-3'
        LANGUAGE 'c' IMMUTABLE;
CREATE OR REPLACE FUNCTION postgis_sfcgal_noop(geometry)
        RETURNS geometry
        AS '$libdir/postgis_sfcgal-3', 'postgis_sfcgal_noop'
        LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
        COST 1;
CREATE OR REPLACE FUNCTION ST_3DIntersection(geom1 geometry, geom2 geometry)
       RETURNS geometry
       AS '$libdir/postgis_sfcgal-3','sfcgal_intersection3D'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;
CREATE OR REPLACE FUNCTION ST_3DDifference(geom1 geometry, geom2 geometry)
       RETURNS geometry
       AS '$libdir/postgis_sfcgal-3','sfcgal_difference3D'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;
CREATE OR REPLACE FUNCTION ST_3DUnion(geom1 geometry, geom2 geometry)
       RETURNS geometry
       AS '$libdir/postgis_sfcgal-3','sfcgal_union3D'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;
-- Aggregate ST_3DUnion(geometry) -- LastUpdated: 303
DO LANGUAGE 'plpgsql'
$postgis_proc_upgrade$
BEGIN
  IF pg_catalog.current_setting('server_version_num')::integer >= 120000
  THEN
    EXECUTE $postgis_proc_upgrade_parsed_def$ CREATE OR REPLACE AGGREGATE ST_3DUnion(geometry) (
       sfunc = ST_3DUnion,
       stype = geometry,
       parallel = safe
);
 $postgis_proc_upgrade_parsed_def$;
  ELSIF 303 > version_from_num OR (
      303 = version_from_num AND version_from_isdev
    ) FROM _postgis_upgrade_info()
  THEN
    EXECUTE 'DROP AGGREGATE IF EXISTS ST_3DUnion(geometry)';
    EXECUTE $postgis_proc_upgrade_parsed_def$ CREATE AGGREGATE ST_3DUnion(geometry) (
       sfunc = ST_3DUnion,
       stype = geometry,
       parallel = safe
);
 $postgis_proc_upgrade_parsed_def$;
  END IF;
END
$postgis_proc_upgrade$;
CREATE OR REPLACE FUNCTION ST_Tesselate(geometry)
       RETURNS geometry
       AS '$libdir/postgis_sfcgal-3','sfcgal_tesselate'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;
CREATE OR REPLACE FUNCTION ST_3DArea(geometry)
       RETURNS FLOAT8
       AS '$libdir/postgis_sfcgal-3','sfcgal_area3D'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;
CREATE OR REPLACE FUNCTION ST_Extrude(geometry, float8, float8, float8)
       RETURNS geometry
       AS '$libdir/postgis_sfcgal-3','sfcgal_extrude'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;
CREATE OR REPLACE FUNCTION ST_ForceLHR(geometry)
       RETURNS geometry
       AS '$libdir/postgis_sfcgal-3','sfcgal_force_lhr'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;
CREATE OR REPLACE FUNCTION ST_Orientation(geometry)
       RETURNS INT4
       AS '$libdir/postgis_sfcgal-3','sfcgal_orientation'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;
CREATE OR REPLACE FUNCTION ST_MinkowskiSum(geometry, geometry)
       RETURNS geometry
       AS '$libdir/postgis_sfcgal-3','sfcgal_minkowski_sum'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;
CREATE OR REPLACE FUNCTION ST_StraightSkeleton(geometry)
       RETURNS geometry
       AS '$libdir/postgis_sfcgal-3','sfcgal_straight_skeleton'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;
CREATE OR REPLACE FUNCTION ST_ApproximateMedialAxis(geometry)
       RETURNS geometry
       AS '$libdir/postgis_sfcgal-3','sfcgal_approximate_medial_axis'
       LANGUAGE 'c'
       IMMUTABLE STRICT PARALLEL SAFE
       COST 100;
CREATE OR REPLACE FUNCTION ST_IsPlanar(geometry)
       RETURNS boolean
       AS '$libdir/postgis_sfcgal-3','sfcgal_is_planar'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;
CREATE OR REPLACE FUNCTION ST_Volume(geometry)
       RETURNS FLOAT8
       AS '$libdir/postgis_sfcgal-3','sfcgal_volume'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;
CREATE OR REPLACE FUNCTION ST_MakeSolid(geometry)
       RETURNS geometry
       AS '$libdir/postgis_sfcgal-3','sfcgal_make_solid'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;
CREATE OR REPLACE FUNCTION ST_IsSolid(geometry)
       RETURNS boolean
       AS '$libdir/postgis_sfcgal-3','sfcgal_is_solid'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;
CREATE OR REPLACE FUNCTION ST_ConstrainedDelaunayTriangles(geometry)
       RETURNS geometry
       AS '$libdir/postgis_sfcgal-3', 'ST_ConstrainedDelaunayTriangles'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;
CREATE OR REPLACE FUNCTION ST_3DConvexHull(geometry)
       RETURNS geometry
       AS '$libdir/postgis_sfcgal-3', 'sfcgal_convexhull3D'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;
CREATE OR REPLACE FUNCTION ST_AlphaShape(g1 geometry, alpha float8 DEFAULT 1.0, allow_holes boolean DEFAULT false)
       RETURNS geometry
       AS '$libdir/postgis_sfcgal-3', 'sfcgal_alphashape'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;
CREATE OR REPLACE FUNCTION ST_OptimalAlphaShape(g1 geometry, allow_holes boolean DEFAULT false, nb_components int DEFAULT 1)
       RETURNS geometry
       AS '$libdir/postgis_sfcgal-3', 'sfcgal_optimalalphashape'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;
-- Drop deprecated functions if possible
DO LANGUAGE 'plpgsql'
$postgis_proc_upgrade$
DECLARE
    deprecated_functions regprocedure[];
    new_name TEXT;
    rewrote_as_wrapper BOOLEAN;
    rec RECORD;
    extrec RECORD;
    procrec RECORD;
    sql TEXT;
    detail TEXT;
    hint TEXT;
BEGIN
    -- Fetch a list of deprecated functions

    SELECT array_agg(oid::regprocedure)
    FROM pg_catalog.pg_proc
    WHERE proname = ANY ('{}'::name[])
    INTO deprecated_functions;

    RAISE DEBUG 'Handling deprecated functions: %', deprecated_functions;

--    -- Rewrite views using deprecated functions
--    -- to improve the odds of being able to drop them
--
--    FOR rec IN
--        SELECT n.nspname AS schemaname,
--            c.relname AS viewname,
--            pg_catalog.pg_get_userbyid(c.relowner) AS viewowner,
--            pg_catalog.pg_get_viewdef(c.oid) AS definition,
--            CASE
--                WHEN 'check_option=cascaded' = ANY (c.reloptions) THEN 'WITH CASCADED CHECK OPTION'
--                WHEN 'check_option=local' = ANY (c.reloptions) THEN 'WITH LOCAL CHECK OPTION'
--                ELSE ''
--            END::text AS check_option
--        FROM pg_catalog.pg_class c
--        LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
--        WHERE c.relkind = 'v'
--        AND pg_catalog.pg_get_viewdef(c.oid) ~ 'deprecated_by_postgis'
--    LOOP
--        sql := pg_catalog.format('CREATE OR REPLACE VIEW %I.%I AS %s %s',
--            rec.schemaname,
--            rec.viewname,
--            pg_catalog.regexp_replace(rec.definition, '_deprecated_by_postgis_[^(]*', '', 'g'),
--            rec.check_option
--        );
--        RAISE NOTICE 'Updating view % to not use deprecated signatures', rec.viewname;
--        BEGIN
--            EXECUTE sql;
--        EXCEPTION
--        WHEN OTHERS THEN
--                GET STACKED DIAGNOSTICS detail := PG_EXCEPTION_DETAIL;
--                RAISE WARNING 'Could not rewrite view % using deprecated functions', rec.viewname
--                        USING DETAIL = pg_catalog.format('%s: %s', SQLERRM, detail);
--        END;
--    END LOOP;

    -- Try to drop all deprecated functions, or rewrite those
    -- who cannot be drop and rewrite them in SQL

    FOR rec IN SELECT pg_catalog.unnest(deprecated_functions) as proc
    LOOP --{

        RAISE DEBUG 'Handling deprecated function %', rec.proc;

        new_name := pg_catalog.regexp_replace(
            rec.proc::text,
            '_deprecated_by_postgis[^(]*\(.*',
            ''
        );

        sql := pg_catalog.format('DROP FUNCTION %s', rec.proc);
        --RAISE DEBUG 'SQL: %', sql;
        BEGIN
            EXECUTE sql;
        EXCEPTION
        WHEN OTHERS THEN
            hint = 'Resolve the issue';
            GET STACKED DIAGNOSTICS detail := PG_EXCEPTION_DETAIL;
            IF detail LIKE '%view % depends%' THEN
                hint = pg_catalog.format(
                    'Replace the view changing all occurrences of %s in its definition with %s',
                    rec.proc,
                    new_name
                );
            END IF;
            hint = hint || ' and upgrade again';

            RAISE WARNING 'Deprecated function % left behind: %',
                rec.proc, SQLERRM
            USING DETAIL = detail, HINT = hint;

            --
            -- Try to rewrite the function as an SQL WRAPPER
            -- {
            SELECT pg_get_functiondef(oid) def, pronargs
            FROM pg_catalog.pg_proc WHERE oid = rec.proc
            INTO procrec;
            --
            -- TODO: don't even try if it's an aggregate or windowing
            --       function (procrec.prokind)
            -- TODO: don't even try if it's a scripting language function
            --       function (procrec.prokind)
            --
            -- Force LANGUAGE to be SQL
            sql := pg_catalog.regexp_replace(procrec.def, 'LANGUAGE [^ 
]*', 'LANGUAGE sql');
            --RAISE DEBUG 'SQL (LANGUAGE): %', sql;
            -- Change body to be a wrapper
            sql := pg_catalog.regexp_replace(
                sql,
                -- Find a stricted match here ?
                'AS .*',
                pg_catalog.format(
                    -- TODO: have the function raise a warning too ?
                    'AS $$ SELECT %s(%s) $$',
                    new_name,
                    (
                        SELECT array_to_string(
                            array_agg('$' || x),
                            ','
                        )
                        FROM generate_series(1, procrec.pronargs) x
                    )
                )
            );
            RAISE DEBUG 'SQL: %', sql;
            rewrote_as_wrapper := false;
            BEGIN
                EXECUTE sql;
                rewrote_as_wrapper := true;
            EXCEPTION
            WHEN OTHERS THEN
                RAISE WARNING
                    'Deprecated function % could not be rewritten as a wrapper: % (%)',
                    rec.proc, SQLERRM, SQLSTATE;
            END;
            --
            --}


            -- Drop the function from any extension it is part of
            -- so dump/reloads still work
            FOR extrec IN
                SELECT e.extname
                FROM
                    pg_catalog.pg_extension e,
                    pg_catalog.pg_depend d
                WHERE
                    d.refclassid = 'pg_catalog.pg_extension'::pg_catalog.regclass AND
                    d.refobjid = e.oid AND
                    d.classid = 'pg_catalog.pg_proc'::pg_catalog.regclass AND
                    d.objid = rec.proc::oid
            LOOP
                RAISE DEBUG 'Unpackaging % from extension %', rec.proc, extrec.extname;
                sql := pg_catalog.format('ALTER EXTENSION %I DROP FUNCTION %s', extrec.extname, rec.proc);
                EXECUTE sql;
            END LOOP;
        END;
    END LOOP; --}
END
$postgis_proc_upgrade$;
DROP FUNCTION _postgis_upgrade_info();
-- PostGIS - Spatial Types for PostgreSQL
-- http://postgis.net
--
-- Copyright (C) 2020 Regina Obe <lr@pcorp.us>
-- This is free software; you can redistribute and/or modify it under
-- the terms of the GNU General Public Licence. See the COPYING file.
--
--
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
-- These are reserved for functions that are changed to use default args
-- This is installed after the new functions are installed
-- We don't have any of these yet for sfcgal
-- The reason we put these after install is
-- you can't drop a function that is used by sql functions
-- without forcing a drop on those as well which may cause issues with user functions.
-- This allows us to CREATE OR REPLACE those in general sfcgal.sql.in
-- without dropping them.




COMMENT ON FUNCTION postgis_sfcgal_version() IS 'Returns the version of SFCGAL in use';
			
COMMENT ON FUNCTION postgis_sfcgal_full_version() IS 'Returns the full version of SFCGAL in use including CGAL and Boost versions';
			
COMMENT ON FUNCTION ST_3DArea(geometry) IS 'args: geom1 - Computes area of 3D surface geometries. Will return 0 for solids.';
			
COMMENT ON FUNCTION ST_3DConvexHull(geometry) IS 'args: geom1 - Computes the 3D convex hull of a geometry.';
			
COMMENT ON FUNCTION ST_3DIntersection(geometry, geometry) IS 'args: geom1, geom2 - Perform 3D intersection';
			
COMMENT ON FUNCTION ST_3DDifference(geometry, geometry) IS 'args: geom1, geom2 - Perform 3D difference';
			
COMMENT ON FUNCTION ST_3DUnion(geometry, geometry) IS 'args: geom1, geom2 - Perform 3D union.';
			
COMMENT ON AGGREGATE ST_3DUnion(geometry) IS 'args: g1field - Perform 3D union.';
			
COMMENT ON FUNCTION ST_AlphaShape(geometry, float , boolean ) IS 'args: geom, alpha, allow_holes = false - Computes a possible concave geometry using the CGAL Alpha Shapes algorithm.';
			
COMMENT ON FUNCTION ST_ApproximateMedialAxis(geometry) IS 'args: geom - Compute the approximate medial axis of an areal geometry.';
			
COMMENT ON FUNCTION ST_ConstrainedDelaunayTriangles(geometry ) IS 'args: g1 - Return a constrained Delaunay triangulation around the given input geometry.';
			
COMMENT ON FUNCTION ST_Extrude(geometry, float, float, float) IS 'args: geom, x, y, z - Extrude a surface to a related volume';
			
COMMENT ON FUNCTION ST_ForceLHR(geometry) IS 'args: geom - Force LHR orientation';
			
COMMENT ON FUNCTION ST_IsPlanar(geometry) IS 'args: geom - Check if a surface is or not planar';
			
COMMENT ON FUNCTION ST_IsSolid(geometry) IS 'args: geom1 - Test if the geometry is a solid. No validity check is performed.';
			
COMMENT ON FUNCTION ST_MakeSolid(geometry) IS 'args: geom1 - Cast the geometry into a solid. No check is performed. To obtain a valid solid, the input geometry must be a closed Polyhedral Surface or a closed TIN.';
			
COMMENT ON FUNCTION ST_MinkowskiSum(geometry, geometry) IS 'args: geom1, geom2 - Performs Minkowski sum';
			
COMMENT ON FUNCTION ST_OptimalAlphaShape(geometry, boolean , integer ) IS 'args: param_geom, allow_holes = false, nb_components - Computes a possible concave geometry using the CGAL Alpha Shapes algorithm after have computed the "optimal" alpha value.';
			
COMMENT ON FUNCTION ST_Orientation(geometry) IS 'args: geom - Determine surface orientation';
			
COMMENT ON FUNCTION ST_StraightSkeleton(geometry) IS 'args: geom - Compute a straight skeleton from a geometry';
			
COMMENT ON FUNCTION ST_Tesselate(geometry) IS 'args: geom - Perform surface Tesselation of a polygon or polyhedralsurface and returns as a TIN or collection of TINS';
			
COMMENT ON FUNCTION ST_Volume(geometry) IS 'args: geom1 - Computes the volume of a 3D solid. If applied to surface (even closed) geometries will return 0.';
			-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
--
----
-- PostGIS - Spatial Types for PostgreSQL
-- http://postgis.net
--
-- Copyright (C) 2011 Regina Obe <lr@pcorp.us>
--
-- This is free software; you can redistribute and/or modify it under
-- the terms of the GNU General Public Licence. See the COPYING file.
--
-- Author: Regina Obe <lr@pcorp.us>
--
-- This drops extension helper functions
-- and should be called at the end of the extension upgrade file
DROP FUNCTION postgis_extension_remove_objects(text, text);
DROP FUNCTION postgis_extension_drop_if_exists(text, text);
DROP FUNCTION IF EXISTS postgis_extension_AddToSearchPath(varchar);
DROP FUNCTION IF EXISTS postgis_extension_AddToSearchPath(text);
