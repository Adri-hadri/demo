---------------------------------------------------------------------------
--
-- PostGIS - SFCGAL functions
-- Copyright 2012-2013 Oslandia <infos@oslandia.com>
--
-- This is free software; you can redistribute and/or modify it under
-- the terms of the GNU General Public Licence. See the COPYING file.
--
---------------------------------------------------------------------------

--
-- New SFCGAL functions (meaning prototype not already provided by GEOS)
--






   




   



   













-- INSTALL VERSION: '3.3.4'



CREATE OR REPLACE FUNCTION postgis_sfcgal_scripts_installed() RETURNS text
	AS $$ SELECT trim('3.3.4'::text || $rev$ 3.3.4 $rev$) AS version $$
	LANGUAGE 'sql' IMMUTABLE;

-- Availability: 2.1.0
CREATE OR REPLACE FUNCTION postgis_sfcgal_version() RETURNS text
        AS '$libdir/postgis_sfcgal-3'
        LANGUAGE 'c' IMMUTABLE;

-- Availability: 3.3.0

CREATE OR REPLACE FUNCTION postgis_sfcgal_full_version() RETURNS text
        AS '$libdir/postgis_sfcgal-3'
        LANGUAGE 'c' IMMUTABLE;


-- Availability: 3.0.0
CREATE OR REPLACE FUNCTION postgis_sfcgal_noop(geometry)
        RETURNS geometry
        AS '$libdir/postgis_sfcgal-3', 'postgis_sfcgal_noop'
        LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
        COST 1;

-- Availability: 2.1.0
CREATE OR REPLACE FUNCTION ST_3DIntersection(geom1 geometry, geom2 geometry)
       RETURNS geometry
       AS '$libdir/postgis_sfcgal-3','sfcgal_intersection3D'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;

-- Availability: 2.2
CREATE OR REPLACE FUNCTION ST_3DDifference(geom1 geometry, geom2 geometry)
       RETURNS geometry
       AS '$libdir/postgis_sfcgal-3','sfcgal_difference3D'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;

-- Availability: 2.2
CREATE OR REPLACE FUNCTION ST_3DUnion(geom1 geometry, geom2 geometry)
       RETURNS geometry
       AS '$libdir/postgis_sfcgal-3','sfcgal_union3D'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;

-- Availability: 3.3.0
CREATE AGGREGATE ST_3DUnion(geometry) (
       sfunc = ST_3DUnion,
       stype = geometry,
       parallel = safe
);

-- Availability: 2.1.0
CREATE OR REPLACE FUNCTION ST_Tesselate(geometry)
       RETURNS geometry
       AS '$libdir/postgis_sfcgal-3','sfcgal_tesselate'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;

-- Availability: 2.1.0
CREATE OR REPLACE FUNCTION ST_3DArea(geometry)
       RETURNS FLOAT8
       AS '$libdir/postgis_sfcgal-3','sfcgal_area3D'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;

-- Availability: 2.1.0
CREATE OR REPLACE FUNCTION ST_Extrude(geometry, float8, float8, float8)
       RETURNS geometry
       AS '$libdir/postgis_sfcgal-3','sfcgal_extrude'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;

-- Availability: 2.1.0
CREATE OR REPLACE FUNCTION ST_ForceLHR(geometry)
       RETURNS geometry
       AS '$libdir/postgis_sfcgal-3','sfcgal_force_lhr'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;

-- Availability: 2.1.0
CREATE OR REPLACE FUNCTION ST_Orientation(geometry)
       RETURNS INT4
       AS '$libdir/postgis_sfcgal-3','sfcgal_orientation'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;

-- Availability: 2.1.0
CREATE OR REPLACE FUNCTION ST_MinkowskiSum(geometry, geometry)
       RETURNS geometry
       AS '$libdir/postgis_sfcgal-3','sfcgal_minkowski_sum'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;

-- Availability: 2.1.0
CREATE OR REPLACE FUNCTION ST_StraightSkeleton(geometry)
       RETURNS geometry
       AS '$libdir/postgis_sfcgal-3','sfcgal_straight_skeleton'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;

-- Availability: 2.2.0
CREATE OR REPLACE FUNCTION ST_ApproximateMedialAxis(geometry)
       RETURNS geometry
       AS '$libdir/postgis_sfcgal-3','sfcgal_approximate_medial_axis'
       LANGUAGE 'c'
       IMMUTABLE STRICT PARALLEL SAFE
       COST 100;

-- Availability: 2.2.0
CREATE OR REPLACE FUNCTION ST_IsPlanar(geometry)
       RETURNS boolean
       AS '$libdir/postgis_sfcgal-3','sfcgal_is_planar'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;

-- Availability: 2.2
CREATE OR REPLACE FUNCTION ST_Volume(geometry)
       RETURNS FLOAT8
       AS '$libdir/postgis_sfcgal-3','sfcgal_volume'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;

-- Availability: 2.2
CREATE OR REPLACE FUNCTION ST_MakeSolid(geometry)
       RETURNS geometry
       AS '$libdir/postgis_sfcgal-3','sfcgal_make_solid'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;

-- Availability: 2.2
CREATE OR REPLACE FUNCTION ST_IsSolid(geometry)
       RETURNS boolean
       AS '$libdir/postgis_sfcgal-3','sfcgal_is_solid'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;

-- Availability: 3.0.0
CREATE OR REPLACE FUNCTION ST_ConstrainedDelaunayTriangles(geometry)
       RETURNS geometry
       AS '$libdir/postgis_sfcgal-3', 'ST_ConstrainedDelaunayTriangles'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;


-- Availability: 3.3.0
CREATE OR REPLACE FUNCTION ST_3DConvexHull(geometry)
       RETURNS geometry
       AS '$libdir/postgis_sfcgal-3', 'sfcgal_convexhull3D'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;


-- Availability: 3.3.0
CREATE OR REPLACE FUNCTION ST_AlphaShape(g1 geometry, alpha float8 DEFAULT 1.0, allow_holes boolean DEFAULT false)
       RETURNS geometry
       AS '$libdir/postgis_sfcgal-3', 'sfcgal_alphashape'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;


-- Availability: 3.3.0
CREATE OR REPLACE FUNCTION ST_OptimalAlphaShape(g1 geometry, allow_holes boolean DEFAULT false, nb_components int DEFAULT 1)
       RETURNS geometry
       AS '$libdir/postgis_sfcgal-3', 'sfcgal_optimalalphashape'
       LANGUAGE 'c' IMMUTABLE STRICT PARALLEL SAFE
       COST 100;



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
			