-- Bare minimum Legacy functions --
-- This file that contains what we have determined are
-- the most common functions used by older apps
-- You should be able to get by with just to support
-- older versions of mapserver, geoserver, qgis, gdal, openjump etc.





   




   



   












-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION AsBinary(geometry)
	RETURNS bytea
	AS '$libdir/postgis-3','LWGEOM_asBinary'
	LANGUAGE 'c' IMMUTABLE STRICT;

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION AsBinary(geometry,text)
	RETURNS bytea
	AS '$libdir/postgis-3','LWGEOM_asBinary'
	LANGUAGE 'c' IMMUTABLE STRICT;

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION AsText(geometry)
	RETURNS TEXT
	AS '$libdir/postgis-3','LWGEOM_asText'
	LANGUAGE 'c' IMMUTABLE STRICT;

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION Estimated_Extent(text,text,text) RETURNS box2d AS
	'$libdir/postgis-3', 'geometry_estimated_extent'
	LANGUAGE 'c' IMMUTABLE STRICT SECURITY DEFINER;

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION Estimated_Extent(text,text) RETURNS box2d AS
	'$libdir/postgis-3', 'geometry_estimated_extent'
	LANGUAGE 'c' IMMUTABLE STRICT SECURITY DEFINER;

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION GeomFromText(text, int4)
	RETURNS geometry AS 'SELECT ST_GeomFromText($1, $2)'
	LANGUAGE 'sql' IMMUTABLE STRICT;

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION GeomFromText(text)
	RETURNS geometry AS 'SELECT ST_GeomFromText($1)'
	LANGUAGE 'sql' IMMUTABLE STRICT;

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION ndims(geometry)
	RETURNS smallint
	AS '$libdir/postgis-3', 'LWGEOM_ndims'
	LANGUAGE 'c' IMMUTABLE STRICT;

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION SetSRID(geometry,int4)
	RETURNS geometry
	AS '$libdir/postgis-3','LWGEOM_set_srid'
	LANGUAGE 'c' IMMUTABLE STRICT;

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION SRID(geometry)
	RETURNS int4
	AS '$libdir/postgis-3','LWGEOM_get_srid'
	LANGUAGE 'c' IMMUTABLE STRICT;

-- Deprecation in 1.5.0
-- hack to allow unknown to cast to geometry
-- so does not yield function is not unique
CREATE OR REPLACE FUNCTION ST_AsBinary(text)
	RETURNS bytea
	AS
	$$ SELECT ST_AsBinary($1::geometry);$$
	LANGUAGE 'sql' IMMUTABLE STRICT;

-- Deprecation in 1.5.0
-- hack to allow unknown to cast to geometry
-- so does not yield function is not unique
CREATE OR REPLACE FUNCTION ST_AsText(bytea)
	RETURNS text
	AS
	$$ SELECT ST_AsText($1::geometry);$$
	LANGUAGE 'sql' IMMUTABLE STRICT;

