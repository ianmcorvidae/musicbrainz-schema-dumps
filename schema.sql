--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: cover_art_archive; Type: SCHEMA; Schema: -; Owner: musicbrainz
--

CREATE SCHEMA cover_art_archive;


ALTER SCHEMA cover_art_archive OWNER TO musicbrainz;

--
-- Name: documentation; Type: SCHEMA; Schema: -; Owner: musicbrainz
--

CREATE SCHEMA documentation;


ALTER SCHEMA documentation OWNER TO musicbrainz;

--
-- Name: musicbrainz; Type: SCHEMA; Schema: -; Owner: musicbrainz
--

CREATE SCHEMA musicbrainz;


ALTER SCHEMA musicbrainz OWNER TO musicbrainz;

--
-- Name: report; Type: SCHEMA; Schema: -; Owner: musicbrainz
--

CREATE SCHEMA report;


ALTER SCHEMA report OWNER TO musicbrainz;

--
-- Name: statistics; Type: SCHEMA; Schema: -; Owner: musicbrainz
--

CREATE SCHEMA statistics;


ALTER SCHEMA statistics OWNER TO musicbrainz;

--
-- Name: wikidocs; Type: SCHEMA; Schema: -; Owner: musicbrainz
--

CREATE SCHEMA wikidocs;


ALTER SCHEMA wikidocs OWNER TO musicbrainz;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


--
-- Name: cube; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS cube WITH SCHEMA public;


--
-- Name: EXTENSION cube; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION cube IS 'data type for multidimensional cubes';


--
-- Name: musicbrainz_collate; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS musicbrainz_collate WITH SCHEMA musicbrainz;


--
-- Name: EXTENSION musicbrainz_collate; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION musicbrainz_collate IS 'Provides Unicode Collation Algorithm support';


--
-- Name: musicbrainz_unaccent; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS musicbrainz_unaccent WITH SCHEMA musicbrainz;


--
-- Name: EXTENSION musicbrainz_unaccent; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION musicbrainz_unaccent IS 'Removes accents from Unicode data';


SET search_path = musicbrainz, pg_catalog;

--
-- Name: cover_art_presence; Type: TYPE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TYPE cover_art_presence AS ENUM (
    'absent',
    'present',
    'darkened'
);


ALTER TYPE musicbrainz.cover_art_presence OWNER TO musicbrainz;

--
-- Name: fluency; Type: TYPE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TYPE fluency AS ENUM (
    'basic',
    'intermediate',
    'advanced',
    'native'
);


ALTER TYPE musicbrainz.fluency OWNER TO musicbrainz;

SET search_path = cover_art_archive, pg_catalog;

--
-- Name: materialize_caa_presence(); Type: FUNCTION; Schema: cover_art_archive; Owner: musicbrainz
--

CREATE FUNCTION materialize_caa_presence() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
    BEGIN
        -- On delete, set the presence flag to 'absent' if there's no more
        -- cover art
        IF TG_OP = 'DELETE' THEN
            IF NOT EXISTS (
                SELECT TRUE FROM cover_art_archive.cover_art
                WHERE release = OLD.release
            ) THEN
                UPDATE musicbrainz.release_meta
                SET cover_art_presence = 'absent'
                WHERE id = OLD.release;
            END IF;
        END IF;

        -- On insert, set the presence flag to 'present' if it was previously
        -- 'absent'
        IF TG_OP = 'INSERT' THEN
            CASE (
                SELECT cover_art_presence FROM musicbrainz.release_meta
                WHERE id = NEW.release
            )
                WHEN 'absent' THEN
                    UPDATE musicbrainz.release_meta
                    SET cover_art_presence = 'present'
                    WHERE id = NEW.release;
                WHEN 'darkened' THEN
                    RAISE EXCEPTION 'This release has been darkened and cannot have new cover art';
                ELSE
            END CASE;
        END IF;

        RETURN NULL;
    END;
$$;


ALTER FUNCTION cover_art_archive.materialize_caa_presence() OWNER TO musicbrainz;

--
-- Name: resequence_cover_art_trigger(); Type: FUNCTION; Schema: cover_art_archive; Owner: musicbrainz
--

CREATE FUNCTION resequence_cover_art_trigger() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
    BEGIN
        IF TG_OP = 'INSERT' OR TG_OP = 'UPDATE' THEN
            PERFORM cover_art_archive.resequence_positions(NEW.release);
        END IF;

        IF (TG_OP = 'DELETE') OR
           (TG_OP = 'UPDATE' AND NEW.release != OLD.release)
        THEN
            PERFORM cover_art_archive.resequence_positions(OLD.release);
        END IF;

        RETURN NULL;
    END;
$$;


ALTER FUNCTION cover_art_archive.resequence_cover_art_trigger() OWNER TO musicbrainz;

--
-- Name: resequence_positions(integer); Type: FUNCTION; Schema: cover_art_archive; Owner: musicbrainz
--

CREATE FUNCTION resequence_positions(release_id integer) RETURNS void
    LANGUAGE plpgsql
    AS $$
    BEGIN
        UPDATE cover_art_archive.cover_art
        SET ordering = recalculated.row_number
        FROM (
            SELECT *,
              row_number() OVER (PARTITION BY release ORDER BY ordering ASC)
            FROM cover_art_archive.cover_art
            WHERE cover_art.release = release_id
        ) recalculated
        WHERE recalculated.id = cover_art.id AND
          recalculated.row_number != cover_art.ordering;
   END;
$$;


ALTER FUNCTION cover_art_archive.resequence_positions(release_id integer) OWNER TO musicbrainz;

SET search_path = musicbrainz, pg_catalog;

--
-- Name: a_del_recording(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION a_del_recording() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    PERFORM dec_ref_count('artist_credit', OLD.artist_credit, 1);
    RETURN NULL;
END;
$$;


ALTER FUNCTION musicbrainz.a_del_recording() OWNER TO musicbrainz;

--
-- Name: a_del_release(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION a_del_release() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    -- decrement ref_count of the name
    PERFORM dec_ref_count('artist_credit', OLD.artist_credit, 1);
    -- decrement release_count of the parent release group
    UPDATE release_group_meta SET release_count = release_count - 1 WHERE id = OLD.release_group;
    RETURN NULL;
END;
$$;


ALTER FUNCTION musicbrainz.a_del_release() OWNER TO musicbrainz;

--
-- Name: a_del_release_event(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION a_del_release_event() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  PERFORM set_release_group_first_release_date(release_group)
  FROM release
  WHERE release.id = OLD.release;
  RETURN NULL;
END;
$$;


ALTER FUNCTION musicbrainz.a_del_release_event() OWNER TO musicbrainz;

--
-- Name: a_del_release_group(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION a_del_release_group() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    PERFORM dec_ref_count('artist_credit', OLD.artist_credit, 1);
    RETURN NULL;
END;
$$;


ALTER FUNCTION musicbrainz.a_del_release_group() OWNER TO musicbrainz;

--
-- Name: a_del_track(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION a_del_track() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    PERFORM dec_ref_count('artist_credit', OLD.artist_credit, 1);
    -- decrement track_count in the parent medium
    UPDATE medium SET track_count = track_count - 1 WHERE id = OLD.medium;
    RETURN NULL;
END;
$$;


ALTER FUNCTION musicbrainz.a_del_track() OWNER TO musicbrainz;

--
-- Name: a_ins_artist(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION a_ins_artist() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    -- add a new entry to the artist_meta table
    INSERT INTO artist_meta (id) VALUES (NEW.id);
    RETURN NULL;
END;
$$;


ALTER FUNCTION musicbrainz.a_ins_artist() OWNER TO musicbrainz;

--
-- Name: a_ins_editor(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION a_ins_editor() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    -- add a new entry to the editor_watch_preference table
    INSERT INTO editor_watch_preferences (editor) VALUES (NEW.id);

    -- by default watch for new official albums
    INSERT INTO editor_watch_release_group_type (editor, release_group_type)
        VALUES (NEW.id, 2);
    INSERT INTO editor_watch_release_status (editor, release_status)
        VALUES (NEW.id, 1);

    RETURN NULL;
END;
$$;


ALTER FUNCTION musicbrainz.a_ins_editor() OWNER TO musicbrainz;

--
-- Name: a_ins_label(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION a_ins_label() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    INSERT INTO label_meta (id) VALUES (NEW.id);
    RETURN NULL;
END;
$$;


ALTER FUNCTION musicbrainz.a_ins_label() OWNER TO musicbrainz;

--
-- Name: a_ins_recording(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION a_ins_recording() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    PERFORM inc_ref_count('artist_credit', NEW.artist_credit, 1);
    INSERT INTO recording_meta (id) VALUES (NEW.id);
    RETURN NULL;
END;
$$;


ALTER FUNCTION musicbrainz.a_ins_recording() OWNER TO musicbrainz;

--
-- Name: a_ins_release(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION a_ins_release() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    -- increment ref_count of the name
    PERFORM inc_ref_count('artist_credit', NEW.artist_credit, 1);
    -- increment release_count of the parent release group
    UPDATE release_group_meta SET release_count = release_count + 1 WHERE id = NEW.release_group;
    -- add new release_meta
    INSERT INTO release_meta (id) VALUES (NEW.id);
    INSERT INTO release_coverart (id) VALUES (NEW.id);
    RETURN NULL;
END;
$$;


ALTER FUNCTION musicbrainz.a_ins_release() OWNER TO musicbrainz;

--
-- Name: a_ins_release_event(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION a_ins_release_event() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  PERFORM set_release_group_first_release_date(release_group)
  FROM release
  WHERE release.id = NEW.release;
  RETURN NULL;
END;
$$;


ALTER FUNCTION musicbrainz.a_ins_release_event() OWNER TO musicbrainz;

--
-- Name: a_ins_release_group(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION a_ins_release_group() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    PERFORM inc_ref_count('artist_credit', NEW.artist_credit, 1);
    INSERT INTO release_group_meta (id) VALUES (NEW.id);
    RETURN NULL;
END;
$$;


ALTER FUNCTION musicbrainz.a_ins_release_group() OWNER TO musicbrainz;

--
-- Name: a_ins_track(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION a_ins_track() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    PERFORM inc_ref_count('artist_credit', NEW.artist_credit, 1);
    -- increment track_count in the parent medium
    UPDATE medium SET track_count = track_count + 1 WHERE id = NEW.medium;
    RETURN NULL;
END;
$$;


ALTER FUNCTION musicbrainz.a_ins_track() OWNER TO musicbrainz;

--
-- Name: a_ins_work(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION a_ins_work() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    INSERT INTO work_meta (id) VALUES (NEW.id);
    RETURN NULL;
END;
$$;


ALTER FUNCTION musicbrainz.a_ins_work() OWNER TO musicbrainz;

--
-- Name: a_upd_edit(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION a_upd_edit() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NEW.status != OLD.status THEN
       UPDATE edit_artist SET status = NEW.status WHERE edit = NEW.id;
       UPDATE edit_label  SET status = NEW.status WHERE edit = NEW.id;
    END IF;
    RETURN NULL;
END;
$$;


ALTER FUNCTION musicbrainz.a_upd_edit() OWNER TO musicbrainz;

--
-- Name: a_upd_recording(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION a_upd_recording() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NEW.artist_credit != OLD.artist_credit THEN
        PERFORM dec_ref_count('artist_credit', OLD.artist_credit, 1);
        PERFORM inc_ref_count('artist_credit', NEW.artist_credit, 1);
    END IF;
    RETURN NULL;
END;
$$;


ALTER FUNCTION musicbrainz.a_upd_recording() OWNER TO musicbrainz;

--
-- Name: a_upd_release(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION a_upd_release() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NEW.artist_credit != OLD.artist_credit THEN
        PERFORM dec_ref_count('artist_credit', OLD.artist_credit, 1);
        PERFORM inc_ref_count('artist_credit', NEW.artist_credit, 1);
    END IF;
    IF NEW.release_group != OLD.release_group THEN
        -- release group is changed, decrement release_count in the original RG, increment in the new one
        UPDATE release_group_meta SET release_count = release_count - 1 WHERE id = OLD.release_group;
        UPDATE release_group_meta SET release_count = release_count + 1 WHERE id = NEW.release_group;
        PERFORM set_release_group_first_release_date(OLD.release_group);
        PERFORM set_release_group_first_release_date(NEW.release_group);
    END IF;
    RETURN NULL;
END;
$$;


ALTER FUNCTION musicbrainz.a_upd_release() OWNER TO musicbrainz;

--
-- Name: a_upd_release_event(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION a_upd_release_event() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  PERFORM set_release_group_first_release_date(release_group)
  FROM release
  WHERE release.id IN (NEW.release, OLD.release);
  RETURN NULL;
END;
$$;


ALTER FUNCTION musicbrainz.a_upd_release_event() OWNER TO musicbrainz;

--
-- Name: a_upd_release_group(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION a_upd_release_group() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NEW.artist_credit != OLD.artist_credit THEN
        PERFORM dec_ref_count('artist_credit', OLD.artist_credit, 1);
        PERFORM inc_ref_count('artist_credit', NEW.artist_credit, 1);
    END IF;
    RETURN NULL;
END;
$$;


ALTER FUNCTION musicbrainz.a_upd_release_group() OWNER TO musicbrainz;

--
-- Name: a_upd_track(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION a_upd_track() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NEW.artist_credit != OLD.artist_credit THEN
        PERFORM dec_ref_count('artist_credit', OLD.artist_credit, 1);
        PERFORM inc_ref_count('artist_credit', NEW.artist_credit, 1);
    END IF;
    IF NEW.medium != OLD.medium THEN
        -- medium is changed, decrement track_count in the original medium, increment in the new one
        UPDATE medium SET track_count = track_count - 1 WHERE id = OLD.medium;
        UPDATE medium SET track_count = track_count + 1 WHERE id = NEW.medium;
    END IF;
    RETURN NULL;
END;
$$;


ALTER FUNCTION musicbrainz.a_upd_track() OWNER TO musicbrainz;

--
-- Name: b_ins_edit_materialize_status(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION b_ins_edit_materialize_status() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    NEW.status = (SELECT status FROM edit WHERE id = NEW.edit);
    RETURN NEW;
END;
$$;


ALTER FUNCTION musicbrainz.b_ins_edit_materialize_status() OWNER TO musicbrainz;

--
-- Name: b_upd_last_updated_table(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION b_upd_last_updated_table() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    NEW.last_updated = NOW();
    RETURN NEW;
END;
$$;


ALTER FUNCTION musicbrainz.b_upd_last_updated_table() OWNER TO musicbrainz;

--
-- Name: controlled_for_whitespace(text); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION controlled_for_whitespace(text) RETURNS boolean
    LANGUAGE sql IMMUTABLE
    SET search_path TO musicbrainz, public
    AS $_$
  SELECT NOT padded_by_whitespace($1) AND whitespace_collapsed($1);
$_$;


ALTER FUNCTION musicbrainz.controlled_for_whitespace(text) OWNER TO musicbrainz;

--
-- Name: create_bounding_cube(integer[], integer); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION create_bounding_cube(durations integer[], fuzzy integer) RETURNS public.cube
    LANGUAGE plpgsql IMMUTABLE
    AS $$
DECLARE
    point    cube;
    str      VARCHAR;
    i        INTEGER;
    dest     INTEGER;
    count    INTEGER;
    dim      CONSTANT INTEGER = 6;
    selected INTEGER[];
    scalers  INTEGER[];
BEGIN

    count = array_upper(durations, 1);
    IF count < dim THEN
        FOR i IN 1..dim LOOP
            selected[i] = 0;
            scalers[i] = 0;
        END LOOP;
        FOR i IN 1..count LOOP
            selected[i] = durations[i];
            scalers[i] = 1;
        END LOOP;
    ELSE
        FOR i IN 1..dim LOOP
            selected[i] = 0;
            scalers[i] = 0;
        END LOOP;
        FOR i IN 1..count LOOP
            dest = (dim * (i-1) / count) + 1;
            selected[dest] = selected[dest] + durations[i];
            scalers[dest] = scalers[dest] + 1;
        END LOOP;
    END IF;

    str = '(';
    FOR i IN 1..dim LOOP
        IF i > 1 THEN
            str = str || ',';
        END IF;
        str = str || cast((selected[i] - (fuzzy * scalers[i])) as text);
    END LOOP;
    str = str || '),(';
    FOR i IN 1..dim LOOP
        IF i > 1 THEN
            str = str || ',';
        END IF;
        str = str || cast((selected[i] + (fuzzy * scalers[i])) as text);
    END LOOP;
    str = str || ')';

    RETURN str::cube;
END;
$$;


ALTER FUNCTION musicbrainz.create_bounding_cube(durations integer[], fuzzy integer) OWNER TO musicbrainz;

--
-- Name: create_cube_from_durations(integer[]); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION create_cube_from_durations(durations integer[]) RETURNS public.cube
    LANGUAGE plpgsql IMMUTABLE
    AS $$
DECLARE
    point    cube;
    str      VARCHAR;
    i        INTEGER;
    count    INTEGER;
    dest     INTEGER;
    dim      CONSTANT INTEGER = 6;
    selected INTEGER[];
BEGIN

    count = array_upper(durations, 1);
    FOR i IN 0..dim LOOP
        selected[i] = 0;
    END LOOP;

    IF count < dim THEN
        FOR i IN 1..count LOOP
            selected[i] = durations[i];
        END LOOP;
    ELSE
        FOR i IN 1..count LOOP
            dest = (dim * (i-1) / count) + 1;
            selected[dest] = selected[dest] + durations[i];
        END LOOP;
    END IF;

    str = '(';
    FOR i IN 1..dim LOOP
        IF i > 1 THEN
            str = str || ',';
        END IF;
        str = str || cast(selected[i] as text);
    END LOOP;
    str = str || ')';

    RETURN str::cube;
END;
$$;


ALTER FUNCTION musicbrainz.create_cube_from_durations(durations integer[]) OWNER TO musicbrainz;

--
-- Name: dec_ref_count(character varying, integer, integer); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION dec_ref_count(tbl character varying, row_id integer, val integer) RETURNS void
    LANGUAGE plpgsql
    AS $$
DECLARE
    ref_count integer;
BEGIN
    -- decrement ref_count for the old name,
    -- or delete it if ref_count would drop to 0
    EXECUTE 'SELECT ref_count FROM ' || tbl || ' WHERE id = ' || row_id || ' FOR UPDATE' INTO ref_count;
    IF ref_count <= val THEN
        EXECUTE 'DELETE FROM ' || tbl || ' WHERE id = ' || row_id;
    ELSE
        EXECUTE 'UPDATE ' || tbl || ' SET ref_count = ref_count - ' || val || ' WHERE id = ' || row_id;
    END IF;
    RETURN;
END;
$$;


ALTER FUNCTION musicbrainz.dec_ref_count(tbl character varying, row_id integer, val integer) OWNER TO musicbrainz;

--
-- Name: del_collection_sub_on_delete(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION del_collection_sub_on_delete() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
  BEGIN
    UPDATE editor_subscribe_collection sub
     SET available = FALSE, last_seen_name = OLD.name
     FROM editor_collection coll
     WHERE sub.collection = OLD.id AND sub.collection = coll.id;

    RETURN OLD;
  END;
$$;


ALTER FUNCTION musicbrainz.del_collection_sub_on_delete() OWNER TO musicbrainz;

--
-- Name: del_collection_sub_on_private(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION del_collection_sub_on_private() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
  BEGIN
    IF NEW.public = FALSE AND OLD.public = TRUE THEN
      UPDATE editor_subscribe_collection sub
       SET available = FALSE, last_seen_name = OLD.name
       FROM editor_collection coll
       WHERE sub.collection = OLD.id AND sub.collection = coll.id
       AND sub.editor != coll.editor;
    END IF;

    RETURN NEW;
  END;
$$;


ALTER FUNCTION musicbrainz.del_collection_sub_on_private() OWNER TO musicbrainz;

--
-- Name: delete_orphaned_recordings(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION delete_orphaned_recordings() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
  BEGIN
    PERFORM TRUE
    FROM recording outer_r
    WHERE id = OLD.recording
      AND edits_pending = 0
      AND NOT EXISTS (
        SELECT TRUE
        FROM edit JOIN edit_recording er ON edit.id = er.edit
        WHERE er.recording = outer_r.id
          AND type IN (71, 207, 218)
          LIMIT 1
      ) AND NOT EXISTS (
        SELECT TRUE FROM track WHERE track.recording = outer_r.id LIMIT 1
      ) AND NOT EXISTS (
        SELECT TRUE FROM l_artist_recording WHERE entity1 = outer_r.id
          UNION ALL
        SELECT TRUE FROM l_label_recording WHERE entity1 = outer_r.id
          UNION ALL
        SELECT TRUE FROM l_recording_recording WHERE entity1 = outer_r.id OR entity0 = outer_r.id
          UNION ALL
        SELECT TRUE FROM l_recording_release WHERE entity0 = outer_r.id
          UNION ALL
        SELECT TRUE FROM l_recording_release_group WHERE entity0 = outer_r.id
          UNION ALL
        SELECT TRUE FROM l_recording_work WHERE entity0 = outer_r.id
          UNION ALL
         SELECT TRUE FROM l_recording_url WHERE entity0 = outer_r.id
      );

    IF FOUND THEN
      -- Remove references from tables that don't change whether or not this recording
      -- is orphaned.
      DELETE FROM isrc WHERE recording = OLD.recording;
      DELETE FROM recording_annotation WHERE recording = OLD.recording;
      DELETE FROM recording_gid_redirect WHERE new_id = OLD.recording;
      DELETE FROM recording_puid WHERE recording = OLD.recording;
      DELETE FROM recording_rating_raw WHERE recording = OLD.recording;
      DELETE FROM recording_tag WHERE recording = OLD.recording;
      DELETE FROM recording_tag_raw WHERE recording = OLD.recording;

      DELETE FROM recording WHERE id = OLD.recording;
    END IF;

    RETURN NULL;
  END;
$$;


ALTER FUNCTION musicbrainz.delete_orphaned_recordings() OWNER TO musicbrainz;

--
-- Name: delete_ratings(text, integer[]); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION delete_ratings(enttype text, ids integer[]) RETURNS TABLE(editor integer, rating smallint)
    LANGUAGE plpgsql
    AS $_$
DECLARE
    tablename TEXT;
BEGIN
    tablename = enttype || '_rating_raw';
    RETURN QUERY
       EXECUTE 'DELETE FROM ' || tablename || ' WHERE ' || enttype || ' = any($1)
                RETURNING editor, rating'
         USING ids;
    RETURN;
END;
$_$;


ALTER FUNCTION musicbrainz.delete_ratings(enttype text, ids integer[]) OWNER TO musicbrainz;

--
-- Name: delete_tags(text, integer[]); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION delete_tags(enttype text, ids integer[]) RETURNS TABLE(editor integer, tag integer)
    LANGUAGE plpgsql
    AS $_$
DECLARE
    tablename TEXT;
BEGIN
    tablename = enttype || '_tag_raw';
    RETURN QUERY
       EXECUTE 'DELETE FROM ' || tablename || ' WHERE ' || enttype || ' = any($1)
                RETURNING editor, tag'
         USING ids;
    RETURN;
END;
$_$;


ALTER FUNCTION musicbrainz.delete_tags(enttype text, ids integer[]) OWNER TO musicbrainz;

--
-- Name: delete_unused_tag(integer); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION delete_unused_tag(tag_id integer) RETURNS void
    LANGUAGE plpgsql
    AS $$
  BEGIN
    DELETE FROM tag WHERE id = tag_id;
  EXCEPTION
    WHEN foreign_key_violation THEN RETURN;
  END;
$$;


ALTER FUNCTION musicbrainz.delete_unused_tag(tag_id integer) OWNER TO musicbrainz;

--
-- Name: delete_unused_url(integer[]); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION delete_unused_url(ids integer[]) RETURNS void
    LANGUAGE plpgsql
    AS $$
DECLARE
  clear_up INTEGER[];
BEGIN
  SELECT ARRAY(
    SELECT id FROM url url_row WHERE id = any(ids)
    AND NOT (
      EXISTS (
        SELECT TRUE FROM l_artist_url
        WHERE entity1 = url_row.id
        LIMIT 1
      ) OR
      EXISTS (
        SELECT TRUE FROM l_label_url
        WHERE entity1 = url_row.id
        LIMIT 1
      ) OR
      EXISTS (
        SELECT TRUE FROM l_recording_url
        WHERE entity1 = url_row.id
        LIMIT 1
      ) OR
      EXISTS (
        SELECT TRUE FROM l_release_url
        WHERE entity1 = url_row.id
        LIMIT 1
      ) OR
      EXISTS (
        SELECT TRUE FROM l_release_group_url
        WHERE entity1 = url_row.id
        LIMIT 1
      ) OR
      EXISTS (
        SELECT TRUE FROM l_url_url
        WHERE entity0 = url_row.id OR entity1 = url_row.id
        LIMIT 1
      ) OR
      EXISTS (
        SELECT TRUE FROM l_url_work
        WHERE entity0 = url_row.id
        LIMIT 1
      )
    )
  ) INTO clear_up;

  DELETE FROM url_gid_redirect WHERE new_id = any(clear_up);
  DELETE FROM url WHERE id = any(clear_up);
END;
$$;


ALTER FUNCTION musicbrainz.delete_unused_url(ids integer[]) OWNER TO musicbrainz;

--
-- Name: deny_special_purpose_deletion(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION deny_special_purpose_deletion() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    RAISE EXCEPTION 'Attempted to delete a special purpose row';
END;
$$;


ALTER FUNCTION musicbrainz.deny_special_purpose_deletion() OWNER TO musicbrainz;

--
-- Name: empty_artists(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION empty_artists() RETURNS SETOF integer
    LANGUAGE sql
    AS $$
  SELECT id FROM artist
  WHERE
    id > 2 AND
    edits_pending = 0 AND
    (
      last_updated < now() - '1 day'::interval OR last_updated is NULL
    )
  EXCEPT
  SELECT artist FROM edit_artist WHERE edit_artist.status = 1
  EXCEPT
  SELECT artist FROM artist_credit_name
  EXCEPT
  SELECT entity1 FROM l_area_artist
  EXCEPT
  SELECT entity0 FROM l_artist_artist
  EXCEPT
  SELECT entity1 FROM l_artist_artist
  EXCEPT
  SELECT entity0 FROM l_artist_label
  EXCEPT
  SELECT entity0 FROM l_artist_recording
  EXCEPT
  SELECT entity0 FROM l_artist_release
  EXCEPT
  SELECT entity0 FROM l_artist_release_group
  EXCEPT
  SELECT entity0 FROM l_artist_url
  EXCEPT
  SELECT entity0 FROM l_artist_work;
$$;


ALTER FUNCTION musicbrainz.empty_artists() OWNER TO musicbrainz;

--
-- Name: empty_labels(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION empty_labels() RETURNS SETOF integer
    LANGUAGE sql
    AS $$
  SELECT id FROM label
  WHERE
    id > 1 AND
    edits_pending = 0 AND
    (
      last_updated < now() - '1 day'::interval OR last_updated is NULL
    )
  EXCEPT
  SELECT label FROM edit_label WHERE edit_label.status = 1
  EXCEPT
  SELECT label FROM release_label
  EXCEPT
  SELECT entity1 FROM l_area_label
  EXCEPT
  SELECT entity1 FROM l_artist_label
  EXCEPT
  SELECT entity1 FROM l_label_label
  EXCEPT
  SELECT entity0 FROM l_label_label
  EXCEPT
  SELECT entity0 FROM l_label_recording
  EXCEPT
  SELECT entity0 FROM l_label_release
  EXCEPT
  SELECT entity0 FROM l_label_release_group
  EXCEPT
  SELECT entity0 FROM l_label_url
  EXCEPT
  SELECT entity0 FROM l_label_work;
$$;


ALTER FUNCTION musicbrainz.empty_labels() OWNER TO musicbrainz;

--
-- Name: empty_release_groups(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION empty_release_groups() RETURNS SETOF integer
    LANGUAGE sql
    AS $$
  SELECT id FROM release_group
  WHERE
    edits_pending = 0 AND
    (
      last_updated < now() - '1 day'::interval OR last_updated is NULL
    )
  EXCEPT
  SELECT release_group
  FROM edit_release_group
  JOIN edit ON (edit.id = edit_release_group.edit)
  WHERE edit.status = 1
  EXCEPT
  SELECT release_group FROM release
  EXCEPT
  SELECT entity1 FROM l_area_release_group
  EXCEPT
  SELECT entity1 FROM l_artist_release_group
  EXCEPT
  SELECT entity1 FROM l_label_release_group
  EXCEPT
  SELECT entity1 FROM l_recording_release_group
  EXCEPT
  SELECT entity1 FROM l_release_release_group
  EXCEPT
  SELECT entity1 FROM l_release_group_release_group
  EXCEPT
  SELECT entity0 FROM l_release_group_release_group
  EXCEPT
  SELECT entity0 FROM l_release_group_url
  EXCEPT
  SELECT entity0 FROM l_release_group_work;
$$;


ALTER FUNCTION musicbrainz.empty_release_groups() OWNER TO musicbrainz;

--
-- Name: empty_works(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION empty_works() RETURNS SETOF integer
    LANGUAGE sql
    AS $$
  SELECT id FROM work
  WHERE
    edits_pending = 0 AND
    (
      last_updated < now() - '1 day'::interval OR last_updated is NULL
    )
  EXCEPT
  SELECT work
  FROM edit_work
  JOIN edit ON (edit.id = edit_work.edit)
  WHERE edit.status = 1
  EXCEPT
  SELECT entity1 FROM l_area_work
  EXCEPT
  SELECT entity1 FROM l_artist_work
  EXCEPT
  SELECT entity1 FROM l_label_work
  EXCEPT
  SELECT entity1 FROM l_recording_work
  EXCEPT
  SELECT entity1 FROM l_release_work
  EXCEPT
  SELECT entity1 FROM l_release_group_work
  EXCEPT
  SELECT entity1 FROM l_url_work
  EXCEPT
  SELECT entity1 FROM l_work_work
  EXCEPT
  SELECT entity0 FROM l_work_work;
$$;


ALTER FUNCTION musicbrainz.empty_works() OWNER TO musicbrainz;

--
-- Name: end_area_implies_ended(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION end_area_implies_ended() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NEW.end_area IS NOT NULL
    THEN
        NEW.ended = TRUE;
    END IF;
    RETURN NEW;
END;
$$;


ALTER FUNCTION musicbrainz.end_area_implies_ended() OWNER TO musicbrainz;

--
-- Name: end_date_implies_ended(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION end_date_implies_ended() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NEW.end_date_year IS NOT NULL OR
       NEW.end_date_month IS NOT NULL OR
       NEW.end_date_day IS NOT NULL
    THEN
        NEW.ended = TRUE;
    END IF;
    RETURN NEW;
END;
$$;


ALTER FUNCTION musicbrainz.end_date_implies_ended() OWNER TO musicbrainz;

--
-- Name: ensure_work_attribute_type_allows_text(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION ensure_work_attribute_type_allows_text() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
  BEGIN
    IF NEW.work_attribute_text IS NOT NULL
        AND NOT EXISTS (
           SELECT TRUE FROM work_attribute_type
		WHERE work_attribute_type.id = NEW.work_attribute_type
		AND free_text
	)
    THEN
        RAISE EXCEPTION 'This attribute type can not contain free text';
    ELSE RETURN NEW;
    END IF;
  END;
$$;


ALTER FUNCTION musicbrainz.ensure_work_attribute_type_allows_text() OWNER TO musicbrainz;

--
-- Name: from_hex(text); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION from_hex(t text) RETURNS integer
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $$
DECLARE
    r RECORD;
BEGIN
    FOR r IN EXECUTE 'SELECT x'''||t||'''::integer AS hex' LOOP
        RETURN r.hex;
    END LOOP;
END
$$;


ALTER FUNCTION musicbrainz.from_hex(t text) OWNER TO musicbrainz;

--
-- Name: generate_uuid_v3(character varying, character varying); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION generate_uuid_v3(namespace character varying, name character varying) RETURNS uuid
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $$
DECLARE
    value varchar(36);
    bytes varchar;
BEGIN
    bytes = md5(decode(namespace, 'hex') || decode(name, 'escape'));
    value = substr(bytes, 1+0, 8);
    value = value || '-';
    value = value || substr(bytes, 1+2*4, 4);
    value = value || '-';
    value = value || lpad(to_hex((from_hex(substr(bytes, 1+2*6, 2)) & 15) | 48), 2, '0');
    value = value || substr(bytes, 1+2*7, 2);
    value = value || '-';
    value = value || lpad(to_hex((from_hex(substr(bytes, 1+2*8, 2)) & 63) | 128), 2, '0');
    value = value || substr(bytes, 1+2*9, 2);
    value = value || '-';
    value = value || substr(bytes, 1+2*10, 12);
    return value::uuid;
END;
$$;


ALTER FUNCTION musicbrainz.generate_uuid_v3(namespace character varying, name character varying) OWNER TO musicbrainz;

--
-- Name: generate_uuid_v4(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION generate_uuid_v4() RETURNS uuid
    LANGUAGE plpgsql
    AS $$
DECLARE
    value VARCHAR(36);
BEGIN
    value =          lpad(to_hex(ceil(random() * 255)::int), 2, '0');
    value = value || lpad(to_hex(ceil(random() * 255)::int), 2, '0');
    value = value || lpad(to_hex(ceil(random() * 255)::int), 2, '0');
    value = value || lpad(to_hex(ceil(random() * 255)::int), 2, '0');
    value = value || '-';
    value = value || lpad(to_hex(ceil(random() * 255)::int), 2, '0');
    value = value || lpad(to_hex(ceil(random() * 255)::int), 2, '0');
    value = value || '-';
    value = value || lpad((to_hex((ceil(random() * 255)::int & 15) | 64)), 2, '0');
    value = value || lpad(to_hex(ceil(random() * 255)::int), 2, '0');
    value = value || '-';
    value = value || lpad((to_hex((ceil(random() * 255)::int & 63) | 128)), 2, '0');
    value = value || lpad(to_hex(ceil(random() * 255)::int), 2, '0');
    value = value || '-';
    value = value || lpad(to_hex(ceil(random() * 255)::int), 2, '0');
    value = value || lpad(to_hex(ceil(random() * 255)::int), 2, '0');
    value = value || lpad(to_hex(ceil(random() * 255)::int), 2, '0');
    value = value || lpad(to_hex(ceil(random() * 255)::int), 2, '0');
    value = value || lpad(to_hex(ceil(random() * 255)::int), 2, '0');
    value = value || lpad(to_hex(ceil(random() * 255)::int), 2, '0');
    RETURN value::uuid;
END;
$$;


ALTER FUNCTION musicbrainz.generate_uuid_v4() OWNER TO musicbrainz;

--
-- Name: inc_ref_count(character varying, integer, integer); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION inc_ref_count(tbl character varying, row_id integer, val integer) RETURNS void
    LANGUAGE plpgsql
    AS $$
BEGIN
    -- increment ref_count for the new name
    EXECUTE 'SELECT ref_count FROM ' || tbl || ' WHERE id = ' || row_id || ' FOR UPDATE';
    EXECUTE 'UPDATE ' || tbl || ' SET ref_count = ref_count + ' || val || ' WHERE id = ' || row_id;
    RETURN;
END;
$$;


ALTER FUNCTION musicbrainz.inc_ref_count(tbl character varying, row_id integer, val integer) OWNER TO musicbrainz;

--
-- Name: inserting_edits_requires_confirmed_email_address(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION inserting_edits_requires_confirmed_email_address() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  IF NOT (
    SELECT email_confirm_date IS NOT NULL AND email_confirm_date <= now()
    FROM editor
    WHERE editor.id = NEW.editor
  ) THEN
    RAISE EXCEPTION 'Editor tried to create edit without a confirmed email address';
  ELSE
    RETURN NEW;
  END IF;
END;
$$;


ALTER FUNCTION musicbrainz.inserting_edits_requires_confirmed_email_address() OWNER TO musicbrainz;

--
-- Name: padded_by_whitespace(text); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION padded_by_whitespace(text) RETURNS boolean
    LANGUAGE sql IMMUTABLE
    AS $_$
  SELECT btrim($1) <> $1;
$_$;


ALTER FUNCTION musicbrainz.padded_by_whitespace(text) OWNER TO musicbrainz;

--
-- Name: page_index(character varying); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION page_index(txt character varying) RETURNS integer
    LANGUAGE plpgsql IMMUTABLE
    AS $$
DECLARE
    input varchar;
    res integer;
    i integer;
    x varchar;
BEGIN
    input := regexp_replace(upper(substr(musicbrainz.musicbrainz_unaccent(txt), 1, 6)), '[^A-Z ]', '_', 'g');
    res := 0;
    FOR i IN 1..6 LOOP
        x := substr(input, i, 1);
        IF x = '_' OR x = '' THEN
            res := (res << 5);
        ELSIF x = ' ' THEN
            res := (res << 5) | 1;
        ELSE
            res := (res << 5) | (ascii(x) - 63);
        END IF;
    END LOOP;
    RETURN res;
END;
$$;


ALTER FUNCTION musicbrainz.page_index(txt character varying) OWNER TO musicbrainz;

--
-- Name: page_index_max(character varying); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION page_index_max(txt character varying) RETURNS integer
    LANGUAGE plpgsql IMMUTABLE
    AS $$
DECLARE
    input varchar;
    res integer;
    i integer;
    x varchar;
BEGIN
    input := regexp_replace(upper(substr(musicbrainz_unaccent(txt), 1, 6)), '[^A-Z ]', '_', 'g');
    res := 0;
    FOR i IN 1..6 LOOP
        x := substr(input, i, 1);
        IF x = '' THEN
            res := (res << 5) | 31;
        ELSIF x = '_' THEN
            res := (res << 5);
        ELSIF x = ' ' THEN
            res := (res << 5) | 1;
        ELSE
            res := (res << 5) | (ascii(x) - 63);
        END IF;
    END LOOP;
    RETURN res;
END;
$$;


ALTER FUNCTION musicbrainz.page_index_max(txt character varying) OWNER TO musicbrainz;

--
-- Name: prevent_invalid_attributes(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION prevent_invalid_attributes() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NOT EXISTS (
        SELECT TRUE
        FROM (VALUES (NEW.link, NEW.attribute_type)) la (link, attribute_type)
        JOIN link l ON l.id = la.link
        JOIN link_type lt ON l.link_type = lt.id
        JOIN link_attribute_type lat ON lat.id = la.attribute_type
        JOIN link_type_attribute_type ltat ON ltat.attribute_type = lat.root AND ltat.link_type = lt.id
    ) THEN
        RAISE EXCEPTION 'Attribute type % is invalid for link %', NEW.attribute_type, NEW.link;
    END IF;
    RETURN NEW;
END;
$$;


ALTER FUNCTION musicbrainz.prevent_invalid_attributes() OWNER TO musicbrainz;

--
-- Name: remove_unused_links(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION remove_unused_links() RETURNS trigger
    LANGUAGE plpgsql
    AS $_$
DECLARE
    other_ars_exist BOOLEAN;
BEGIN
    EXECUTE 'SELECT EXISTS (SELECT TRUE FROM ' || quote_ident(TG_TABLE_NAME) ||
            ' WHERE link = $1)'
    INTO other_ars_exist
    USING OLD.link;

    IF NOT other_ars_exist THEN
       DELETE FROM link_attribute WHERE link = OLD.link;
       DELETE FROM link WHERE id = OLD.link;
    END IF;

    RETURN NULL;
END;
$_$;


ALTER FUNCTION musicbrainz.remove_unused_links() OWNER TO musicbrainz;

--
-- Name: remove_unused_url(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION remove_unused_url() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF TG_TABLE_NAME LIKE 'l_url_%' THEN
      EXECUTE delete_unused_url(ARRAY[OLD.entity0]);
    END IF;

    IF TG_TABLE_NAME LIKE 'l_%_url' THEN
      EXECUTE delete_unused_url(ARRAY[OLD.entity1]);
    END IF;

    RETURN NULL;
END;
$$;


ALTER FUNCTION musicbrainz.remove_unused_url() OWNER TO musicbrainz;

--
-- Name: replace_old_sub_on_add(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION replace_old_sub_on_add() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
  BEGIN
    UPDATE editor_subscribe_collection
     SET available = TRUE, last_seen_name = NULL,
      last_edit_sent = NEW.last_edit_sent
     WHERE editor = NEW.editor AND collection = NEW.collection;

    IF FOUND THEN
      RETURN NULL;
    ELSE
      RETURN NEW;
    END IF;
  END;
$$;


ALTER FUNCTION musicbrainz.replace_old_sub_on_add() OWNER TO musicbrainz;

--
-- Name: set_release_group_first_release_date(integer); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION set_release_group_first_release_date(release_group_id integer) RETURNS void
    LANGUAGE plpgsql
    AS $$
BEGIN
    UPDATE release_group_meta SET first_release_date_year = first.date_year,
                                  first_release_date_month = first.date_month,
                                  first_release_date_day = first.date_day
      FROM (
        SELECT date_year, date_month, date_day
        FROM (
          SELECT date_year, date_month, date_day
          FROM release
          JOIN release_country ON (release_country.release = release.id)
          WHERE release.release_group = release_group_id
          UNION
          SELECT date_year, date_month, date_day
          FROM release
          JOIN release_unknown_country ON (release_unknown_country.release = release.id)
          WHERE release.release_group = release_group_id
        ) b
        ORDER BY date_year NULLS LAST, date_month NULLS LAST, date_day NULLS LAST
        LIMIT 1
      ) AS first
    WHERE id = release_group_id;
END;
$$;


ALTER FUNCTION musicbrainz.set_release_group_first_release_date(release_group_id integer) OWNER TO musicbrainz;

--
-- Name: simplify_search_hints(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION simplify_search_hints() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NEW.type::int = TG_ARGV[0]::int THEN
        NEW.sort_name := NEW.name;
        NEW.begin_date_year := NULL;
        NEW.begin_date_month := NULL;
        NEW.begin_date_day := NULL;
        NEW.end_date_year := NULL;
        NEW.end_date_month := NULL;
        NEW.end_date_day := NULL;
        NEW.end_date_day := NULL;
        NEW.locale := NULL;
    END IF;
    RETURN NEW;
END;
$$;


ALTER FUNCTION musicbrainz.simplify_search_hints() OWNER TO musicbrainz;

--
-- Name: trg_delete_unused_tag(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION trg_delete_unused_tag() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
  BEGIN
    PERFORM delete_unused_tag(NEW.id);
    RETURN NULL;
  END;
$$;


ALTER FUNCTION musicbrainz.trg_delete_unused_tag() OWNER TO musicbrainz;

--
-- Name: trg_delete_unused_tag_ref(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION trg_delete_unused_tag_ref() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
  BEGIN
    PERFORM delete_unused_tag(OLD.tag);
    RETURN NULL;
  END;
$$;


ALTER FUNCTION musicbrainz.trg_delete_unused_tag_ref() OWNER TO musicbrainz;

--
-- Name: unique_primary_area_alias(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION unique_primary_area_alias() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NEW.primary_for_locale THEN
      UPDATE area_alias SET primary_for_locale = FALSE
      WHERE locale = NEW.locale AND id != NEW.id
        AND area = NEW.area;
    END IF;
    RETURN NEW;
END;
$$;


ALTER FUNCTION musicbrainz.unique_primary_area_alias() OWNER TO musicbrainz;

--
-- Name: unique_primary_artist_alias(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION unique_primary_artist_alias() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NEW.primary_for_locale THEN
      UPDATE artist_alias SET primary_for_locale = FALSE
      WHERE locale = NEW.locale AND id != NEW.id
        AND artist = NEW.artist;
    END IF;
    RETURN NEW;
END;
$$;


ALTER FUNCTION musicbrainz.unique_primary_artist_alias() OWNER TO musicbrainz;

--
-- Name: unique_primary_label_alias(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION unique_primary_label_alias() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NEW.primary_for_locale THEN
      UPDATE label_alias SET primary_for_locale = FALSE
      WHERE locale = NEW.locale AND id != NEW.id
        AND label = NEW.label;
    END IF;
    RETURN NEW;
END;
$$;


ALTER FUNCTION musicbrainz.unique_primary_label_alias() OWNER TO musicbrainz;

--
-- Name: unique_primary_work_alias(); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION unique_primary_work_alias() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NEW.primary_for_locale THEN
      UPDATE work_alias SET primary_for_locale = FALSE
      WHERE locale = NEW.locale AND id != NEW.id
        AND work = NEW.work;
    END IF;
    RETURN NEW;
END;
$$;


ALTER FUNCTION musicbrainz.unique_primary_work_alias() OWNER TO musicbrainz;

--
-- Name: whitespace_collapsed(text); Type: FUNCTION; Schema: musicbrainz; Owner: musicbrainz
--

CREATE FUNCTION whitespace_collapsed(text) RETURNS boolean
    LANGUAGE sql IMMUTABLE
    AS $_$
  SELECT $1 !~ E'\\s{2,}';
$_$;


ALTER FUNCTION musicbrainz.whitespace_collapsed(text) OWNER TO musicbrainz;

--
-- Name: array_accum(anyelement); Type: AGGREGATE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE AGGREGATE array_accum(anyelement) (
    SFUNC = array_append,
    STYPE = anyarray,
    INITCOND = '{}'
);


ALTER AGGREGATE musicbrainz.array_accum(anyelement) OWNER TO musicbrainz;

--
-- Name: mb_simple; Type: TEXT SEARCH CONFIGURATION; Schema: musicbrainz; Owner: postgres
--

CREATE TEXT SEARCH CONFIGURATION mb_simple (
    PARSER = pg_catalog."default" );

ALTER TEXT SEARCH CONFIGURATION mb_simple
    ADD MAPPING FOR asciiword WITH simple;

ALTER TEXT SEARCH CONFIGURATION mb_simple
    ADD MAPPING FOR word WITH musicbrainz_unaccentdict, simple;

ALTER TEXT SEARCH CONFIGURATION mb_simple
    ADD MAPPING FOR numword WITH musicbrainz_unaccentdict, simple;

ALTER TEXT SEARCH CONFIGURATION mb_simple
    ADD MAPPING FOR email WITH simple;

ALTER TEXT SEARCH CONFIGURATION mb_simple
    ADD MAPPING FOR url WITH simple;

ALTER TEXT SEARCH CONFIGURATION mb_simple
    ADD MAPPING FOR host WITH simple;

ALTER TEXT SEARCH CONFIGURATION mb_simple
    ADD MAPPING FOR sfloat WITH simple;

ALTER TEXT SEARCH CONFIGURATION mb_simple
    ADD MAPPING FOR version WITH simple;

ALTER TEXT SEARCH CONFIGURATION mb_simple
    ADD MAPPING FOR hword_numpart WITH musicbrainz_unaccentdict, simple;

ALTER TEXT SEARCH CONFIGURATION mb_simple
    ADD MAPPING FOR hword_part WITH musicbrainz_unaccentdict, simple;

ALTER TEXT SEARCH CONFIGURATION mb_simple
    ADD MAPPING FOR hword_asciipart WITH simple;

ALTER TEXT SEARCH CONFIGURATION mb_simple
    ADD MAPPING FOR numhword WITH musicbrainz_unaccentdict, simple;

ALTER TEXT SEARCH CONFIGURATION mb_simple
    ADD MAPPING FOR asciihword WITH simple;

ALTER TEXT SEARCH CONFIGURATION mb_simple
    ADD MAPPING FOR hword WITH musicbrainz_unaccentdict, simple;

ALTER TEXT SEARCH CONFIGURATION mb_simple
    ADD MAPPING FOR url_path WITH simple;

ALTER TEXT SEARCH CONFIGURATION mb_simple
    ADD MAPPING FOR file WITH simple;

ALTER TEXT SEARCH CONFIGURATION mb_simple
    ADD MAPPING FOR "float" WITH simple;

ALTER TEXT SEARCH CONFIGURATION mb_simple
    ADD MAPPING FOR "int" WITH simple;

ALTER TEXT SEARCH CONFIGURATION mb_simple
    ADD MAPPING FOR uint WITH simple;


ALTER TEXT SEARCH CONFIGURATION musicbrainz.mb_simple OWNER TO postgres;

SET search_path = cover_art_archive, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: art_type; Type: TABLE; Schema: cover_art_archive; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE art_type (
    id integer NOT NULL,
    name text NOT NULL
);


ALTER TABLE cover_art_archive.art_type OWNER TO musicbrainz;

--
-- Name: art_type_id_seq; Type: SEQUENCE; Schema: cover_art_archive; Owner: musicbrainz
--

CREATE SEQUENCE art_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE cover_art_archive.art_type_id_seq OWNER TO musicbrainz;

--
-- Name: art_type_id_seq; Type: SEQUENCE OWNED BY; Schema: cover_art_archive; Owner: musicbrainz
--

ALTER SEQUENCE art_type_id_seq OWNED BY art_type.id;


--
-- Name: cover_art; Type: TABLE; Schema: cover_art_archive; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE cover_art (
    id bigint NOT NULL,
    release integer NOT NULL,
    comment text DEFAULT ''::text NOT NULL,
    edit integer NOT NULL,
    ordering integer NOT NULL,
    date_uploaded timestamp with time zone DEFAULT now() NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    mime_type text NOT NULL,
    CONSTRAINT cover_art_edits_pending_check CHECK ((edits_pending >= 0)),
    CONSTRAINT cover_art_ordering_check CHECK ((ordering > 0))
);


ALTER TABLE cover_art_archive.cover_art OWNER TO musicbrainz;

--
-- Name: cover_art_type; Type: TABLE; Schema: cover_art_archive; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE cover_art_type (
    id bigint NOT NULL,
    type_id integer NOT NULL
);


ALTER TABLE cover_art_archive.cover_art_type OWNER TO musicbrainz;

--
-- Name: image_type; Type: TABLE; Schema: cover_art_archive; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE image_type (
    mime_type text NOT NULL,
    suffix text NOT NULL
);


ALTER TABLE cover_art_archive.image_type OWNER TO musicbrainz;

SET search_path = musicbrainz, pg_catalog;

--
-- Name: edit; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE edit (
    id integer NOT NULL,
    editor integer NOT NULL,
    type smallint NOT NULL,
    status smallint NOT NULL,
    data text NOT NULL,
    yes_votes integer DEFAULT 0 NOT NULL,
    no_votes integer DEFAULT 0 NOT NULL,
    autoedit smallint DEFAULT 0 NOT NULL,
    open_time timestamp with time zone DEFAULT now(),
    close_time timestamp with time zone,
    expire_time timestamp with time zone NOT NULL,
    language integer,
    quality smallint DEFAULT 1 NOT NULL
);


ALTER TABLE musicbrainz.edit OWNER TO musicbrainz;

SET search_path = cover_art_archive, pg_catalog;

--
-- Name: index_listing; Type: VIEW; Schema: cover_art_archive; Owner: musicbrainz
--

CREATE VIEW index_listing AS
    SELECT cover_art.id, cover_art.release, cover_art.comment, cover_art.edit, cover_art.ordering, cover_art.date_uploaded, cover_art.edits_pending, cover_art.mime_type, (edit.close_time IS NOT NULL) AS approved, COALESCE((cover_art.id = (SELECT cover_art_type.id FROM (cover_art_type JOIN cover_art ca_front USING (id)) WHERE ((ca_front.release = cover_art.release) AND (cover_art_type.type_id = 1)) ORDER BY ca_front.ordering LIMIT 1)), false) AS is_front, COALESCE((cover_art.id = (SELECT cover_art_type.id FROM (cover_art_type JOIN cover_art ca_front USING (id)) WHERE ((ca_front.release = cover_art.release) AND (cover_art_type.type_id = 2)) ORDER BY ca_front.ordering LIMIT 1)), false) AS is_back, ARRAY(SELECT art_type.name FROM (cover_art_type JOIN art_type ON ((cover_art_type.type_id = art_type.id))) WHERE (cover_art_type.id = cover_art.id)) AS types FROM (cover_art LEFT JOIN musicbrainz.edit ON ((edit.id = cover_art.edit)));


ALTER TABLE cover_art_archive.index_listing OWNER TO musicbrainz;

--
-- Name: release_group_cover_art; Type: TABLE; Schema: cover_art_archive; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_group_cover_art (
    release_group integer NOT NULL,
    release integer NOT NULL
);


ALTER TABLE cover_art_archive.release_group_cover_art OWNER TO musicbrainz;

SET search_path = documentation, pg_catalog;

--
-- Name: l_area_area_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_area_area_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_area_area_example OWNER TO musicbrainz;

--
-- Name: l_area_artist_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_area_artist_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_area_artist_example OWNER TO musicbrainz;

--
-- Name: l_area_label_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_area_label_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_area_label_example OWNER TO musicbrainz;

--
-- Name: l_area_recording_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_area_recording_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_area_recording_example OWNER TO musicbrainz;

--
-- Name: l_area_release_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_area_release_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_area_release_example OWNER TO musicbrainz;

--
-- Name: l_area_release_group_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_area_release_group_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_area_release_group_example OWNER TO musicbrainz;

--
-- Name: l_area_url_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_area_url_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_area_url_example OWNER TO musicbrainz;

--
-- Name: l_area_work_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_area_work_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_area_work_example OWNER TO musicbrainz;

--
-- Name: l_artist_artist_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_artist_artist_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_artist_artist_example OWNER TO musicbrainz;

--
-- Name: l_artist_label_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_artist_label_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_artist_label_example OWNER TO musicbrainz;

--
-- Name: l_artist_recording_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_artist_recording_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_artist_recording_example OWNER TO musicbrainz;

--
-- Name: l_artist_release_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_artist_release_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_artist_release_example OWNER TO musicbrainz;

--
-- Name: l_artist_release_group_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_artist_release_group_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_artist_release_group_example OWNER TO musicbrainz;

--
-- Name: l_artist_url_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_artist_url_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_artist_url_example OWNER TO musicbrainz;

--
-- Name: l_artist_work_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_artist_work_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_artist_work_example OWNER TO musicbrainz;

--
-- Name: l_label_label_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_label_label_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_label_label_example OWNER TO musicbrainz;

--
-- Name: l_label_recording_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_label_recording_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_label_recording_example OWNER TO musicbrainz;

--
-- Name: l_label_release_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_label_release_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_label_release_example OWNER TO musicbrainz;

--
-- Name: l_label_release_group_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_label_release_group_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_label_release_group_example OWNER TO musicbrainz;

--
-- Name: l_label_url_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_label_url_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_label_url_example OWNER TO musicbrainz;

--
-- Name: l_label_work_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_label_work_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_label_work_example OWNER TO musicbrainz;

--
-- Name: l_recording_recording_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_recording_recording_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_recording_recording_example OWNER TO musicbrainz;

--
-- Name: l_recording_release_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_recording_release_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_recording_release_example OWNER TO musicbrainz;

--
-- Name: l_recording_release_group_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_recording_release_group_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_recording_release_group_example OWNER TO musicbrainz;

--
-- Name: l_recording_url_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_recording_url_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_recording_url_example OWNER TO musicbrainz;

--
-- Name: l_recording_work_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_recording_work_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_recording_work_example OWNER TO musicbrainz;

--
-- Name: l_release_group_release_group_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_release_group_release_group_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_release_group_release_group_example OWNER TO musicbrainz;

--
-- Name: l_release_group_url_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_release_group_url_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_release_group_url_example OWNER TO musicbrainz;

--
-- Name: l_release_group_work_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_release_group_work_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_release_group_work_example OWNER TO musicbrainz;

--
-- Name: l_release_release_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_release_release_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_release_release_example OWNER TO musicbrainz;

--
-- Name: l_release_release_group_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_release_release_group_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_release_release_group_example OWNER TO musicbrainz;

--
-- Name: l_release_url_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_release_url_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_release_url_example OWNER TO musicbrainz;

--
-- Name: l_release_work_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_release_work_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_release_work_example OWNER TO musicbrainz;

--
-- Name: l_url_url_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_url_url_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_url_url_example OWNER TO musicbrainz;

--
-- Name: l_url_work_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_url_work_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_url_work_example OWNER TO musicbrainz;

--
-- Name: l_work_work_example; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_work_work_example (
    id integer NOT NULL,
    published boolean NOT NULL,
    name text NOT NULL
);


ALTER TABLE documentation.l_work_work_example OWNER TO musicbrainz;

--
-- Name: link_type_documentation; Type: TABLE; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE link_type_documentation (
    id integer NOT NULL,
    documentation text NOT NULL,
    examples_deleted smallint DEFAULT 0 NOT NULL
);


ALTER TABLE documentation.link_type_documentation OWNER TO musicbrainz;

SET search_path = musicbrainz, pg_catalog;

--
-- Name: annotation; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE annotation (
    id integer NOT NULL,
    editor integer NOT NULL,
    text text,
    changelog character varying(255),
    created timestamp with time zone DEFAULT now()
);


ALTER TABLE musicbrainz.annotation OWNER TO musicbrainz;

--
-- Name: annotation_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE annotation_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.annotation_id_seq OWNER TO musicbrainz;

--
-- Name: annotation_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE annotation_id_seq OWNED BY annotation.id;


--
-- Name: application; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE application (
    id integer NOT NULL,
    owner integer NOT NULL,
    name text NOT NULL,
    oauth_id text NOT NULL,
    oauth_secret text NOT NULL,
    oauth_redirect_uri text
);


ALTER TABLE musicbrainz.application OWNER TO musicbrainz;

--
-- Name: application_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE application_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.application_id_seq OWNER TO musicbrainz;

--
-- Name: application_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE application_id_seq OWNED BY application.id;


--
-- Name: area; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE area (
    id integer NOT NULL,
    gid uuid NOT NULL,
    name character varying NOT NULL,
    sort_name character varying NOT NULL,
    type integer,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    begin_date_year smallint,
    begin_date_month smallint,
    begin_date_day smallint,
    end_date_year smallint,
    end_date_month smallint,
    end_date_day smallint,
    ended boolean DEFAULT false NOT NULL,
    CONSTRAINT area_check CHECK ((((((end_date_year IS NOT NULL) OR (end_date_month IS NOT NULL)) OR (end_date_day IS NOT NULL)) AND (ended = true)) OR (((end_date_year IS NULL) AND (end_date_month IS NULL)) AND (end_date_day IS NULL)))),
    CONSTRAINT area_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.area OWNER TO musicbrainz;

--
-- Name: area_alias; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE area_alias (
    id integer NOT NULL,
    area integer NOT NULL,
    name character varying NOT NULL,
    locale text,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    type integer,
    sort_name character varying NOT NULL,
    begin_date_year smallint,
    begin_date_month smallint,
    begin_date_day smallint,
    end_date_year smallint,
    end_date_month smallint,
    end_date_day smallint,
    primary_for_locale boolean DEFAULT false NOT NULL,
    ended boolean DEFAULT false NOT NULL,
    CONSTRAINT area_alias_edits_pending_check CHECK ((edits_pending >= 0)),
    CONSTRAINT area_alias_ended_check CHECK ((((((end_date_year IS NOT NULL) OR (end_date_month IS NOT NULL)) OR (end_date_day IS NOT NULL)) AND (ended = true)) OR (((end_date_year IS NULL) AND (end_date_month IS NULL)) AND (end_date_day IS NULL)))),
    CONSTRAINT primary_check CHECK ((((locale IS NULL) AND (primary_for_locale IS FALSE)) OR (locale IS NOT NULL)))
);


ALTER TABLE musicbrainz.area_alias OWNER TO musicbrainz;

--
-- Name: area_alias_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE area_alias_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.area_alias_id_seq OWNER TO musicbrainz;

--
-- Name: area_alias_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE area_alias_id_seq OWNED BY area_alias.id;


--
-- Name: area_alias_type; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE area_alias_type (
    id integer NOT NULL,
    name text NOT NULL
);


ALTER TABLE musicbrainz.area_alias_type OWNER TO musicbrainz;

--
-- Name: area_alias_type_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE area_alias_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.area_alias_type_id_seq OWNER TO musicbrainz;

--
-- Name: area_alias_type_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE area_alias_type_id_seq OWNED BY area_alias_type.id;


--
-- Name: area_annotation; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE area_annotation (
    area integer NOT NULL,
    annotation integer NOT NULL
);


ALTER TABLE musicbrainz.area_annotation OWNER TO musicbrainz;

--
-- Name: area_gid_redirect; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE area_gid_redirect (
    gid uuid NOT NULL,
    new_id integer NOT NULL,
    created timestamp with time zone DEFAULT now()
);


ALTER TABLE musicbrainz.area_gid_redirect OWNER TO musicbrainz;

--
-- Name: area_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE area_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.area_id_seq OWNER TO musicbrainz;

--
-- Name: area_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE area_id_seq OWNED BY area.id;


--
-- Name: area_type; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE area_type (
    id integer NOT NULL,
    name character varying(255) NOT NULL
);


ALTER TABLE musicbrainz.area_type OWNER TO musicbrainz;

--
-- Name: area_type_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE area_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.area_type_id_seq OWNER TO musicbrainz;

--
-- Name: area_type_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE area_type_id_seq OWNED BY area_type.id;


--
-- Name: artist; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE artist (
    id integer NOT NULL,
    gid uuid NOT NULL,
    name character varying NOT NULL,
    sort_name character varying NOT NULL,
    begin_date_year smallint,
    begin_date_month smallint,
    begin_date_day smallint,
    end_date_year smallint,
    end_date_month smallint,
    end_date_day smallint,
    type integer,
    area integer,
    gender integer,
    comment character varying(255) DEFAULT ''::character varying NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    ended boolean DEFAULT false NOT NULL,
    begin_area integer,
    end_area integer,
    CONSTRAINT artist_comment_check CHECK (controlled_for_whitespace((comment)::text)),
    CONSTRAINT artist_edits_pending_check CHECK ((edits_pending >= 0)),
    CONSTRAINT artist_ended_check CHECK ((((((end_date_year IS NOT NULL) OR (end_date_month IS NOT NULL)) OR (end_date_day IS NOT NULL)) AND (ended = true)) OR (((end_date_year IS NULL) AND (end_date_month IS NULL)) AND (end_date_day IS NULL)))),
    CONSTRAINT artist_va_check CHECK (((id <> 1) OR (((((((((((type = 3) AND (gender IS NULL)) AND (area IS NULL)) AND (begin_area IS NULL)) AND (end_area IS NULL)) AND (begin_date_year IS NULL)) AND (begin_date_month IS NULL)) AND (begin_date_day IS NULL)) AND (end_date_year IS NULL)) AND (end_date_month IS NULL)) AND (end_date_day IS NULL)))),
    CONSTRAINT control_for_whitespace CHECK (controlled_for_whitespace((name)::text)),
    CONSTRAINT control_for_whitespace_sort_name CHECK (controlled_for_whitespace((sort_name)::text)),
    CONSTRAINT group_type_implies_null_gender CHECK ((((gender IS NULL) AND (type = 2)) OR (type IS DISTINCT FROM 2))),
    CONSTRAINT only_non_empty CHECK (((name)::text <> ''::text)),
    CONSTRAINT only_non_empty_sort_name CHECK (((sort_name)::text <> ''::text))
);


ALTER TABLE musicbrainz.artist OWNER TO musicbrainz;

--
-- Name: artist_alias; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE artist_alias (
    id integer NOT NULL,
    artist integer NOT NULL,
    name character varying NOT NULL,
    locale text,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    type integer,
    sort_name character varying NOT NULL,
    begin_date_year smallint,
    begin_date_month smallint,
    begin_date_day smallint,
    end_date_year smallint,
    end_date_month smallint,
    end_date_day smallint,
    primary_for_locale boolean DEFAULT false NOT NULL,
    ended boolean DEFAULT false NOT NULL,
    CONSTRAINT artist_alias_edits_pending_check CHECK ((edits_pending >= 0)),
    CONSTRAINT artist_alias_ended_check CHECK ((((((end_date_year IS NOT NULL) OR (end_date_month IS NOT NULL)) OR (end_date_day IS NOT NULL)) AND (ended = true)) OR (((end_date_year IS NULL) AND (end_date_month IS NULL)) AND (end_date_day IS NULL)))),
    CONSTRAINT control_for_whitespace CHECK (controlled_for_whitespace((name)::text)),
    CONSTRAINT control_for_whitespace_sort_name CHECK (controlled_for_whitespace((sort_name)::text)),
    CONSTRAINT only_non_empty CHECK (((name)::text <> ''::text)),
    CONSTRAINT only_non_empty_sort_name CHECK (((sort_name)::text <> ''::text)),
    CONSTRAINT primary_check CHECK ((((locale IS NULL) AND (primary_for_locale IS FALSE)) OR (locale IS NOT NULL))),
    CONSTRAINT search_hints_are_empty CHECK (((type <> 3) OR ((((((((((type = 3) AND ((sort_name)::text = (name)::text)) AND (begin_date_year IS NULL)) AND (begin_date_month IS NULL)) AND (begin_date_day IS NULL)) AND (end_date_year IS NULL)) AND (end_date_month IS NULL)) AND (end_date_day IS NULL)) AND (primary_for_locale IS FALSE)) AND (locale IS NULL))))
);


ALTER TABLE musicbrainz.artist_alias OWNER TO musicbrainz;

--
-- Name: artist_alias_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE artist_alias_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.artist_alias_id_seq OWNER TO musicbrainz;

--
-- Name: artist_alias_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE artist_alias_id_seq OWNED BY artist_alias.id;


--
-- Name: artist_alias_type; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE artist_alias_type (
    id integer NOT NULL,
    name text NOT NULL
);


ALTER TABLE musicbrainz.artist_alias_type OWNER TO musicbrainz;

--
-- Name: artist_alias_type_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE artist_alias_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.artist_alias_type_id_seq OWNER TO musicbrainz;

--
-- Name: artist_alias_type_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE artist_alias_type_id_seq OWNED BY artist_alias_type.id;


--
-- Name: artist_annotation; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE artist_annotation (
    artist integer NOT NULL,
    annotation integer NOT NULL
);


ALTER TABLE musicbrainz.artist_annotation OWNER TO musicbrainz;

--
-- Name: artist_credit; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE artist_credit (
    id integer NOT NULL,
    name character varying NOT NULL,
    artist_count smallint NOT NULL,
    ref_count integer DEFAULT 0,
    created timestamp with time zone DEFAULT now(),
    CONSTRAINT control_for_whitespace CHECK (controlled_for_whitespace((name)::text)),
    CONSTRAINT only_non_empty CHECK (((name)::text <> ''::text))
);


ALTER TABLE musicbrainz.artist_credit OWNER TO musicbrainz;

--
-- Name: artist_credit_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE artist_credit_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.artist_credit_id_seq OWNER TO musicbrainz;

--
-- Name: artist_credit_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE artist_credit_id_seq OWNED BY artist_credit.id;


--
-- Name: artist_credit_name; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE artist_credit_name (
    artist_credit integer NOT NULL,
    "position" smallint NOT NULL,
    artist integer NOT NULL,
    name character varying NOT NULL,
    join_phrase text DEFAULT ''::text NOT NULL,
    CONSTRAINT control_for_whitespace CHECK (controlled_for_whitespace((name)::text)),
    CONSTRAINT only_non_empty CHECK (((name)::text <> ''::text))
);


ALTER TABLE musicbrainz.artist_credit_name OWNER TO musicbrainz;

--
-- Name: artist_deletion; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE artist_deletion (
    gid uuid NOT NULL,
    last_known_name character varying NOT NULL,
    last_known_comment text NOT NULL,
    deleted_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE musicbrainz.artist_deletion OWNER TO musicbrainz;

--
-- Name: artist_gid_redirect; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE artist_gid_redirect (
    gid uuid NOT NULL,
    new_id integer NOT NULL,
    created timestamp with time zone DEFAULT now()
);


ALTER TABLE musicbrainz.artist_gid_redirect OWNER TO musicbrainz;

--
-- Name: artist_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE artist_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.artist_id_seq OWNER TO musicbrainz;

--
-- Name: artist_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE artist_id_seq OWNED BY artist.id;


--
-- Name: artist_ipi; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE artist_ipi (
    artist integer NOT NULL,
    ipi character(11) NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    created timestamp with time zone DEFAULT now(),
    CONSTRAINT artist_ipi_edits_pending_check CHECK ((edits_pending >= 0)),
    CONSTRAINT artist_ipi_ipi_check CHECK ((ipi ~ '^\d{11}$'::text))
);


ALTER TABLE musicbrainz.artist_ipi OWNER TO musicbrainz;

--
-- Name: artist_isni; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE artist_isni (
    artist integer NOT NULL,
    isni character(16) NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    created timestamp with time zone DEFAULT now(),
    CONSTRAINT artist_isni_edits_pending_check CHECK ((edits_pending >= 0)),
    CONSTRAINT artist_isni_isni_check CHECK ((isni ~ '^\d{15}[\dX]$'::text))
);


ALTER TABLE musicbrainz.artist_isni OWNER TO musicbrainz;

--
-- Name: artist_meta; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE artist_meta (
    id integer NOT NULL,
    rating smallint,
    rating_count integer,
    CONSTRAINT artist_meta_rating_check CHECK (((rating >= 0) AND (rating <= 100)))
);


ALTER TABLE musicbrainz.artist_meta OWNER TO musicbrainz;

--
-- Name: artist_rating_raw; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE artist_rating_raw (
    artist integer NOT NULL,
    editor integer NOT NULL,
    rating smallint NOT NULL,
    CONSTRAINT artist_rating_raw_rating_check CHECK (((rating >= 0) AND (rating <= 100)))
);


ALTER TABLE musicbrainz.artist_rating_raw OWNER TO musicbrainz;

--
-- Name: artist_tag; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE artist_tag (
    artist integer NOT NULL,
    tag integer NOT NULL,
    count integer NOT NULL,
    last_updated timestamp with time zone DEFAULT now()
);


ALTER TABLE musicbrainz.artist_tag OWNER TO musicbrainz;

--
-- Name: artist_tag_raw; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE artist_tag_raw (
    artist integer NOT NULL,
    editor integer NOT NULL,
    tag integer NOT NULL
);


ALTER TABLE musicbrainz.artist_tag_raw OWNER TO musicbrainz;

--
-- Name: artist_type; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE artist_type (
    id integer NOT NULL,
    name character varying(255) NOT NULL
);


ALTER TABLE musicbrainz.artist_type OWNER TO musicbrainz;

--
-- Name: artist_type_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE artist_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.artist_type_id_seq OWNER TO musicbrainz;

--
-- Name: artist_type_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE artist_type_id_seq OWNED BY artist_type.id;


--
-- Name: autoeditor_election; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE autoeditor_election (
    id integer NOT NULL,
    candidate integer NOT NULL,
    proposer integer NOT NULL,
    seconder_1 integer,
    seconder_2 integer,
    status integer DEFAULT 1 NOT NULL,
    yes_votes integer DEFAULT 0 NOT NULL,
    no_votes integer DEFAULT 0 NOT NULL,
    propose_time timestamp with time zone DEFAULT now() NOT NULL,
    open_time timestamp with time zone,
    close_time timestamp with time zone,
    CONSTRAINT autoeditor_election_status_check CHECK ((status = ANY (ARRAY[1, 2, 3, 4, 5, 6])))
);


ALTER TABLE musicbrainz.autoeditor_election OWNER TO musicbrainz;

--
-- Name: autoeditor_election_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE autoeditor_election_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.autoeditor_election_id_seq OWNER TO musicbrainz;

--
-- Name: autoeditor_election_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE autoeditor_election_id_seq OWNED BY autoeditor_election.id;


--
-- Name: autoeditor_election_vote; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE autoeditor_election_vote (
    id integer NOT NULL,
    autoeditor_election integer NOT NULL,
    voter integer NOT NULL,
    vote integer NOT NULL,
    vote_time timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT autoeditor_election_vote_vote_check CHECK ((vote = ANY (ARRAY[(-1), 0, 1])))
);


ALTER TABLE musicbrainz.autoeditor_election_vote OWNER TO musicbrainz;

--
-- Name: autoeditor_election_vote_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE autoeditor_election_vote_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.autoeditor_election_vote_id_seq OWNER TO musicbrainz;

--
-- Name: autoeditor_election_vote_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE autoeditor_election_vote_id_seq OWNED BY autoeditor_election_vote.id;


--
-- Name: cdtoc; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE cdtoc (
    id integer NOT NULL,
    discid character(28) NOT NULL,
    freedb_id character(8) NOT NULL,
    track_count integer NOT NULL,
    leadout_offset integer NOT NULL,
    track_offset integer[] NOT NULL,
    degraded boolean DEFAULT false NOT NULL,
    created timestamp with time zone DEFAULT now()
);


ALTER TABLE musicbrainz.cdtoc OWNER TO musicbrainz;

--
-- Name: cdtoc_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE cdtoc_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.cdtoc_id_seq OWNER TO musicbrainz;

--
-- Name: cdtoc_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE cdtoc_id_seq OWNED BY cdtoc.id;


--
-- Name: cdtoc_raw; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE cdtoc_raw (
    id integer NOT NULL,
    release integer NOT NULL,
    discid character(28) NOT NULL,
    track_count integer NOT NULL,
    leadout_offset integer NOT NULL,
    track_offset integer[] NOT NULL
);


ALTER TABLE musicbrainz.cdtoc_raw OWNER TO musicbrainz;

--
-- Name: cdtoc_raw_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE cdtoc_raw_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.cdtoc_raw_id_seq OWNER TO musicbrainz;

--
-- Name: cdtoc_raw_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE cdtoc_raw_id_seq OWNED BY cdtoc_raw.id;


--
-- Name: country_area; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE country_area (
    area integer NOT NULL
);


ALTER TABLE musicbrainz.country_area OWNER TO musicbrainz;

--
-- Name: edit_area; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE edit_area (
    edit integer NOT NULL,
    area integer NOT NULL
);


ALTER TABLE musicbrainz.edit_area OWNER TO musicbrainz;

--
-- Name: edit_artist; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE edit_artist (
    edit integer NOT NULL,
    artist integer NOT NULL,
    status smallint NOT NULL
);


ALTER TABLE musicbrainz.edit_artist OWNER TO musicbrainz;

--
-- Name: edit_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE edit_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.edit_id_seq OWNER TO musicbrainz;

--
-- Name: edit_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE edit_id_seq OWNED BY edit.id;


--
-- Name: edit_label; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE edit_label (
    edit integer NOT NULL,
    label integer NOT NULL,
    status smallint NOT NULL
);


ALTER TABLE musicbrainz.edit_label OWNER TO musicbrainz;

--
-- Name: edit_note; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE edit_note (
    id integer NOT NULL,
    editor integer NOT NULL,
    edit integer NOT NULL,
    text text NOT NULL,
    post_time timestamp with time zone DEFAULT now()
);


ALTER TABLE musicbrainz.edit_note OWNER TO musicbrainz;

--
-- Name: edit_note_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE edit_note_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.edit_note_id_seq OWNER TO musicbrainz;

--
-- Name: edit_note_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE edit_note_id_seq OWNED BY edit_note.id;


--
-- Name: edit_recording; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE edit_recording (
    edit integer NOT NULL,
    recording integer NOT NULL
);


ALTER TABLE musicbrainz.edit_recording OWNER TO musicbrainz;

--
-- Name: edit_release; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE edit_release (
    edit integer NOT NULL,
    release integer NOT NULL
);


ALTER TABLE musicbrainz.edit_release OWNER TO musicbrainz;

--
-- Name: edit_release_group; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE edit_release_group (
    edit integer NOT NULL,
    release_group integer NOT NULL
);


ALTER TABLE musicbrainz.edit_release_group OWNER TO musicbrainz;

--
-- Name: edit_url; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE edit_url (
    edit integer NOT NULL,
    url integer NOT NULL
);


ALTER TABLE musicbrainz.edit_url OWNER TO musicbrainz;

--
-- Name: edit_work; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE edit_work (
    edit integer NOT NULL,
    work integer NOT NULL
);


ALTER TABLE musicbrainz.edit_work OWNER TO musicbrainz;

--
-- Name: editor; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE editor (
    id integer NOT NULL,
    name character varying(64) NOT NULL,
    privs integer DEFAULT 0,
    email character varying(64) DEFAULT NULL::character varying,
    website character varying(255) DEFAULT NULL::character varying,
    bio text,
    member_since timestamp with time zone DEFAULT now(),
    email_confirm_date timestamp with time zone,
    last_login_date timestamp with time zone DEFAULT now(),
    edits_accepted integer DEFAULT 0,
    edits_rejected integer DEFAULT 0,
    auto_edits_accepted integer DEFAULT 0,
    edits_failed integer DEFAULT 0,
    last_updated timestamp with time zone DEFAULT now(),
    birth_date date,
    gender integer,
    area integer,
    password character varying(128) NOT NULL,
    ha1 character(32) NOT NULL,
    deleted boolean DEFAULT false NOT NULL
);


ALTER TABLE musicbrainz.editor OWNER TO musicbrainz;

--
-- Name: editor_collection; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE editor_collection (
    id integer NOT NULL,
    gid uuid NOT NULL,
    editor integer NOT NULL,
    name character varying NOT NULL,
    public boolean DEFAULT false NOT NULL,
    description text DEFAULT ''::text NOT NULL
);


ALTER TABLE musicbrainz.editor_collection OWNER TO musicbrainz;

--
-- Name: editor_collection_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE editor_collection_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.editor_collection_id_seq OWNER TO musicbrainz;

--
-- Name: editor_collection_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE editor_collection_id_seq OWNED BY editor_collection.id;


--
-- Name: editor_collection_release; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE editor_collection_release (
    collection integer NOT NULL,
    release integer NOT NULL
);


ALTER TABLE musicbrainz.editor_collection_release OWNER TO musicbrainz;

--
-- Name: editor_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE editor_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.editor_id_seq OWNER TO musicbrainz;

--
-- Name: editor_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE editor_id_seq OWNED BY editor.id;


--
-- Name: editor_language; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE editor_language (
    editor integer NOT NULL,
    language integer NOT NULL,
    fluency fluency NOT NULL
);


ALTER TABLE musicbrainz.editor_language OWNER TO musicbrainz;

--
-- Name: editor_oauth_token; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE editor_oauth_token (
    id integer NOT NULL,
    editor integer NOT NULL,
    application integer NOT NULL,
    authorization_code text,
    refresh_token text,
    access_token text,
    mac_key text,
    mac_time_diff integer,
    expire_time timestamp with time zone NOT NULL,
    scope integer DEFAULT 0 NOT NULL,
    granted timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE musicbrainz.editor_oauth_token OWNER TO musicbrainz;

--
-- Name: editor_oauth_token_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE editor_oauth_token_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.editor_oauth_token_id_seq OWNER TO musicbrainz;

--
-- Name: editor_oauth_token_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE editor_oauth_token_id_seq OWNED BY editor_oauth_token.id;


--
-- Name: editor_preference; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE editor_preference (
    id integer NOT NULL,
    editor integer NOT NULL,
    name character varying(50) NOT NULL,
    value character varying(100) NOT NULL
);


ALTER TABLE musicbrainz.editor_preference OWNER TO musicbrainz;

--
-- Name: editor_preference_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE editor_preference_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.editor_preference_id_seq OWNER TO musicbrainz;

--
-- Name: editor_preference_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE editor_preference_id_seq OWNED BY editor_preference.id;


--
-- Name: editor_subscribe_artist; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE editor_subscribe_artist (
    id integer NOT NULL,
    editor integer NOT NULL,
    artist integer NOT NULL,
    last_edit_sent integer NOT NULL
);


ALTER TABLE musicbrainz.editor_subscribe_artist OWNER TO musicbrainz;

--
-- Name: editor_subscribe_artist_deleted; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE editor_subscribe_artist_deleted (
    editor integer NOT NULL,
    gid uuid NOT NULL,
    deleted_by integer NOT NULL
);


ALTER TABLE musicbrainz.editor_subscribe_artist_deleted OWNER TO musicbrainz;

--
-- Name: editor_subscribe_artist_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE editor_subscribe_artist_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.editor_subscribe_artist_id_seq OWNER TO musicbrainz;

--
-- Name: editor_subscribe_artist_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE editor_subscribe_artist_id_seq OWNED BY editor_subscribe_artist.id;


--
-- Name: editor_subscribe_collection; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE editor_subscribe_collection (
    id integer NOT NULL,
    editor integer NOT NULL,
    collection integer NOT NULL,
    last_edit_sent integer NOT NULL,
    available boolean DEFAULT true NOT NULL,
    last_seen_name character varying(255)
);


ALTER TABLE musicbrainz.editor_subscribe_collection OWNER TO musicbrainz;

--
-- Name: editor_subscribe_collection_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE editor_subscribe_collection_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.editor_subscribe_collection_id_seq OWNER TO musicbrainz;

--
-- Name: editor_subscribe_collection_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE editor_subscribe_collection_id_seq OWNED BY editor_subscribe_collection.id;


--
-- Name: editor_subscribe_editor; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE editor_subscribe_editor (
    id integer NOT NULL,
    editor integer NOT NULL,
    subscribed_editor integer NOT NULL,
    last_edit_sent integer NOT NULL
);


ALTER TABLE musicbrainz.editor_subscribe_editor OWNER TO musicbrainz;

--
-- Name: editor_subscribe_editor_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE editor_subscribe_editor_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.editor_subscribe_editor_id_seq OWNER TO musicbrainz;

--
-- Name: editor_subscribe_editor_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE editor_subscribe_editor_id_seq OWNED BY editor_subscribe_editor.id;


--
-- Name: editor_subscribe_label; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE editor_subscribe_label (
    id integer NOT NULL,
    editor integer NOT NULL,
    label integer NOT NULL,
    last_edit_sent integer NOT NULL
);


ALTER TABLE musicbrainz.editor_subscribe_label OWNER TO musicbrainz;

--
-- Name: editor_subscribe_label_deleted; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE editor_subscribe_label_deleted (
    editor integer NOT NULL,
    gid uuid NOT NULL,
    deleted_by integer NOT NULL
);


ALTER TABLE musicbrainz.editor_subscribe_label_deleted OWNER TO musicbrainz;

--
-- Name: editor_subscribe_label_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE editor_subscribe_label_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.editor_subscribe_label_id_seq OWNER TO musicbrainz;

--
-- Name: editor_subscribe_label_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE editor_subscribe_label_id_seq OWNED BY editor_subscribe_label.id;


--
-- Name: editor_watch_artist; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE editor_watch_artist (
    artist integer NOT NULL,
    editor integer NOT NULL
);


ALTER TABLE musicbrainz.editor_watch_artist OWNER TO musicbrainz;

--
-- Name: editor_watch_preferences; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE editor_watch_preferences (
    editor integer NOT NULL,
    notify_via_email boolean DEFAULT true NOT NULL,
    notification_timeframe interval DEFAULT '7 days'::interval NOT NULL,
    last_checked timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE musicbrainz.editor_watch_preferences OWNER TO musicbrainz;

--
-- Name: editor_watch_release_group_type; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE editor_watch_release_group_type (
    editor integer NOT NULL,
    release_group_type integer NOT NULL
);


ALTER TABLE musicbrainz.editor_watch_release_group_type OWNER TO musicbrainz;

--
-- Name: editor_watch_release_status; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE editor_watch_release_status (
    editor integer NOT NULL,
    release_status integer NOT NULL
);


ALTER TABLE musicbrainz.editor_watch_release_status OWNER TO musicbrainz;

--
-- Name: gender; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE gender (
    id integer NOT NULL,
    name character varying(255) NOT NULL
);


ALTER TABLE musicbrainz.gender OWNER TO musicbrainz;

--
-- Name: gender_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE gender_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.gender_id_seq OWNER TO musicbrainz;

--
-- Name: gender_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE gender_id_seq OWNED BY gender.id;


--
-- Name: iso_3166_1; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE iso_3166_1 (
    area integer NOT NULL,
    code character(2) NOT NULL
);


ALTER TABLE musicbrainz.iso_3166_1 OWNER TO musicbrainz;

--
-- Name: iso_3166_2; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE iso_3166_2 (
    area integer NOT NULL,
    code character varying(10) NOT NULL
);


ALTER TABLE musicbrainz.iso_3166_2 OWNER TO musicbrainz;

--
-- Name: iso_3166_3; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE iso_3166_3 (
    area integer NOT NULL,
    code character(4) NOT NULL
);


ALTER TABLE musicbrainz.iso_3166_3 OWNER TO musicbrainz;

--
-- Name: isrc; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE isrc (
    id integer NOT NULL,
    recording integer NOT NULL,
    isrc character(12) NOT NULL,
    source smallint,
    edits_pending integer DEFAULT 0 NOT NULL,
    created timestamp with time zone DEFAULT now(),
    CONSTRAINT isrc_edits_pending_check CHECK ((edits_pending >= 0)),
    CONSTRAINT isrc_isrc_check CHECK ((isrc ~ '^[A-Z]{2}[A-Z0-9]{3}[0-9]{7}$'::text))
);


ALTER TABLE musicbrainz.isrc OWNER TO musicbrainz;

--
-- Name: isrc_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE isrc_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.isrc_id_seq OWNER TO musicbrainz;

--
-- Name: isrc_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE isrc_id_seq OWNED BY isrc.id;


--
-- Name: iswc; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE iswc (
    id integer NOT NULL,
    work integer NOT NULL,
    iswc character(15),
    source smallint,
    edits_pending integer DEFAULT 0 NOT NULL,
    created timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT iswc_iswc_check CHECK ((iswc ~ '^T-?\d{3}.?\d{3}.?\d{3}[-.]?\d$'::text))
);


ALTER TABLE musicbrainz.iswc OWNER TO musicbrainz;

--
-- Name: iswc_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE iswc_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.iswc_id_seq OWNER TO musicbrainz;

--
-- Name: iswc_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE iswc_id_seq OWNED BY iswc.id;


--
-- Name: l_area_area; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_area_area (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_area_area_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_area_area OWNER TO musicbrainz;

--
-- Name: l_area_area_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_area_area_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_area_area_id_seq OWNER TO musicbrainz;

--
-- Name: l_area_area_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_area_area_id_seq OWNED BY l_area_area.id;


--
-- Name: l_area_artist; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_area_artist (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_area_artist_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_area_artist OWNER TO musicbrainz;

--
-- Name: l_area_artist_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_area_artist_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_area_artist_id_seq OWNER TO musicbrainz;

--
-- Name: l_area_artist_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_area_artist_id_seq OWNED BY l_area_artist.id;


--
-- Name: l_area_label; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_area_label (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_area_label_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_area_label OWNER TO musicbrainz;

--
-- Name: l_area_label_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_area_label_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_area_label_id_seq OWNER TO musicbrainz;

--
-- Name: l_area_label_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_area_label_id_seq OWNED BY l_area_label.id;


--
-- Name: l_area_recording; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_area_recording (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_area_recording_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_area_recording OWNER TO musicbrainz;

--
-- Name: l_area_recording_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_area_recording_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_area_recording_id_seq OWNER TO musicbrainz;

--
-- Name: l_area_recording_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_area_recording_id_seq OWNED BY l_area_recording.id;


--
-- Name: l_area_release; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_area_release (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_area_release_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_area_release OWNER TO musicbrainz;

--
-- Name: l_area_release_group; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_area_release_group (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_area_release_group_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_area_release_group OWNER TO musicbrainz;

--
-- Name: l_area_release_group_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_area_release_group_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_area_release_group_id_seq OWNER TO musicbrainz;

--
-- Name: l_area_release_group_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_area_release_group_id_seq OWNED BY l_area_release_group.id;


--
-- Name: l_area_release_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_area_release_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_area_release_id_seq OWNER TO musicbrainz;

--
-- Name: l_area_release_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_area_release_id_seq OWNED BY l_area_release.id;


--
-- Name: l_area_url; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_area_url (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_area_url_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_area_url OWNER TO musicbrainz;

--
-- Name: l_area_url_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_area_url_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_area_url_id_seq OWNER TO musicbrainz;

--
-- Name: l_area_url_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_area_url_id_seq OWNED BY l_area_url.id;


--
-- Name: l_area_work; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_area_work (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_area_work_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_area_work OWNER TO musicbrainz;

--
-- Name: l_area_work_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_area_work_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_area_work_id_seq OWNER TO musicbrainz;

--
-- Name: l_area_work_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_area_work_id_seq OWNED BY l_area_work.id;


--
-- Name: l_artist_artist; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_artist_artist (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_artist_artist_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_artist_artist OWNER TO musicbrainz;

--
-- Name: l_artist_artist_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_artist_artist_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_artist_artist_id_seq OWNER TO musicbrainz;

--
-- Name: l_artist_artist_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_artist_artist_id_seq OWNED BY l_artist_artist.id;


--
-- Name: l_artist_label; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_artist_label (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_artist_label_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_artist_label OWNER TO musicbrainz;

--
-- Name: l_artist_label_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_artist_label_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_artist_label_id_seq OWNER TO musicbrainz;

--
-- Name: l_artist_label_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_artist_label_id_seq OWNED BY l_artist_label.id;


--
-- Name: l_artist_recording; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_artist_recording (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_artist_recording_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_artist_recording OWNER TO musicbrainz;

--
-- Name: l_artist_recording_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_artist_recording_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_artist_recording_id_seq OWNER TO musicbrainz;

--
-- Name: l_artist_recording_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_artist_recording_id_seq OWNED BY l_artist_recording.id;


--
-- Name: l_artist_release; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_artist_release (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_artist_release_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_artist_release OWNER TO musicbrainz;

--
-- Name: l_artist_release_group; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_artist_release_group (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_artist_release_group_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_artist_release_group OWNER TO musicbrainz;

--
-- Name: l_artist_release_group_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_artist_release_group_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_artist_release_group_id_seq OWNER TO musicbrainz;

--
-- Name: l_artist_release_group_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_artist_release_group_id_seq OWNED BY l_artist_release_group.id;


--
-- Name: l_artist_release_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_artist_release_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_artist_release_id_seq OWNER TO musicbrainz;

--
-- Name: l_artist_release_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_artist_release_id_seq OWNED BY l_artist_release.id;


--
-- Name: l_artist_url; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_artist_url (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_artist_url_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_artist_url OWNER TO musicbrainz;

--
-- Name: l_artist_url_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_artist_url_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_artist_url_id_seq OWNER TO musicbrainz;

--
-- Name: l_artist_url_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_artist_url_id_seq OWNED BY l_artist_url.id;


--
-- Name: l_artist_work; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_artist_work (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_artist_work_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_artist_work OWNER TO musicbrainz;

--
-- Name: l_artist_work_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_artist_work_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_artist_work_id_seq OWNER TO musicbrainz;

--
-- Name: l_artist_work_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_artist_work_id_seq OWNED BY l_artist_work.id;


--
-- Name: l_label_label; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_label_label (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_label_label_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_label_label OWNER TO musicbrainz;

--
-- Name: l_label_label_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_label_label_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_label_label_id_seq OWNER TO musicbrainz;

--
-- Name: l_label_label_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_label_label_id_seq OWNED BY l_label_label.id;


--
-- Name: l_label_recording; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_label_recording (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_label_recording_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_label_recording OWNER TO musicbrainz;

--
-- Name: l_label_recording_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_label_recording_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_label_recording_id_seq OWNER TO musicbrainz;

--
-- Name: l_label_recording_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_label_recording_id_seq OWNED BY l_label_recording.id;


--
-- Name: l_label_release; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_label_release (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_label_release_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_label_release OWNER TO musicbrainz;

--
-- Name: l_label_release_group; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_label_release_group (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_label_release_group_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_label_release_group OWNER TO musicbrainz;

--
-- Name: l_label_release_group_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_label_release_group_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_label_release_group_id_seq OWNER TO musicbrainz;

--
-- Name: l_label_release_group_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_label_release_group_id_seq OWNED BY l_label_release_group.id;


--
-- Name: l_label_release_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_label_release_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_label_release_id_seq OWNER TO musicbrainz;

--
-- Name: l_label_release_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_label_release_id_seq OWNED BY l_label_release.id;


--
-- Name: l_label_url; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_label_url (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_label_url_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_label_url OWNER TO musicbrainz;

--
-- Name: l_label_url_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_label_url_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_label_url_id_seq OWNER TO musicbrainz;

--
-- Name: l_label_url_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_label_url_id_seq OWNED BY l_label_url.id;


--
-- Name: l_label_work; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_label_work (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_label_work_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_label_work OWNER TO musicbrainz;

--
-- Name: l_label_work_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_label_work_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_label_work_id_seq OWNER TO musicbrainz;

--
-- Name: l_label_work_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_label_work_id_seq OWNED BY l_label_work.id;


--
-- Name: l_recording_recording; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_recording_recording (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_recording_recording_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_recording_recording OWNER TO musicbrainz;

--
-- Name: l_recording_recording_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_recording_recording_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_recording_recording_id_seq OWNER TO musicbrainz;

--
-- Name: l_recording_recording_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_recording_recording_id_seq OWNED BY l_recording_recording.id;


--
-- Name: l_recording_release; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_recording_release (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_recording_release_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_recording_release OWNER TO musicbrainz;

--
-- Name: l_recording_release_group; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_recording_release_group (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_recording_release_group_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_recording_release_group OWNER TO musicbrainz;

--
-- Name: l_recording_release_group_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_recording_release_group_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_recording_release_group_id_seq OWNER TO musicbrainz;

--
-- Name: l_recording_release_group_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_recording_release_group_id_seq OWNED BY l_recording_release_group.id;


--
-- Name: l_recording_release_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_recording_release_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_recording_release_id_seq OWNER TO musicbrainz;

--
-- Name: l_recording_release_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_recording_release_id_seq OWNED BY l_recording_release.id;


--
-- Name: l_recording_url; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_recording_url (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_recording_url_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_recording_url OWNER TO musicbrainz;

--
-- Name: l_recording_url_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_recording_url_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_recording_url_id_seq OWNER TO musicbrainz;

--
-- Name: l_recording_url_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_recording_url_id_seq OWNED BY l_recording_url.id;


--
-- Name: l_recording_work; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_recording_work (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_recording_work_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_recording_work OWNER TO musicbrainz;

--
-- Name: l_recording_work_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_recording_work_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_recording_work_id_seq OWNER TO musicbrainz;

--
-- Name: l_recording_work_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_recording_work_id_seq OWNED BY l_recording_work.id;


--
-- Name: l_release_group_release_group; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_release_group_release_group (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_release_group_release_group_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_release_group_release_group OWNER TO musicbrainz;

--
-- Name: l_release_group_release_group_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_release_group_release_group_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_release_group_release_group_id_seq OWNER TO musicbrainz;

--
-- Name: l_release_group_release_group_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_release_group_release_group_id_seq OWNED BY l_release_group_release_group.id;


--
-- Name: l_release_group_url; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_release_group_url (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_release_group_url_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_release_group_url OWNER TO musicbrainz;

--
-- Name: l_release_group_url_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_release_group_url_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_release_group_url_id_seq OWNER TO musicbrainz;

--
-- Name: l_release_group_url_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_release_group_url_id_seq OWNED BY l_release_group_url.id;


--
-- Name: l_release_group_work; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_release_group_work (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_release_group_work_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_release_group_work OWNER TO musicbrainz;

--
-- Name: l_release_group_work_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_release_group_work_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_release_group_work_id_seq OWNER TO musicbrainz;

--
-- Name: l_release_group_work_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_release_group_work_id_seq OWNED BY l_release_group_work.id;


--
-- Name: l_release_release; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_release_release (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_release_release_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_release_release OWNER TO musicbrainz;

--
-- Name: l_release_release_group; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_release_release_group (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_release_release_group_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_release_release_group OWNER TO musicbrainz;

--
-- Name: l_release_release_group_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_release_release_group_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_release_release_group_id_seq OWNER TO musicbrainz;

--
-- Name: l_release_release_group_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_release_release_group_id_seq OWNED BY l_release_release_group.id;


--
-- Name: l_release_release_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_release_release_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_release_release_id_seq OWNER TO musicbrainz;

--
-- Name: l_release_release_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_release_release_id_seq OWNED BY l_release_release.id;


--
-- Name: l_release_url; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_release_url (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_release_url_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_release_url OWNER TO musicbrainz;

--
-- Name: l_release_url_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_release_url_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_release_url_id_seq OWNER TO musicbrainz;

--
-- Name: l_release_url_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_release_url_id_seq OWNED BY l_release_url.id;


--
-- Name: l_release_work; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_release_work (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_release_work_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_release_work OWNER TO musicbrainz;

--
-- Name: l_release_work_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_release_work_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_release_work_id_seq OWNER TO musicbrainz;

--
-- Name: l_release_work_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_release_work_id_seq OWNED BY l_release_work.id;


--
-- Name: l_url_url; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_url_url (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_url_url_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_url_url OWNER TO musicbrainz;

--
-- Name: l_url_url_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_url_url_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_url_url_id_seq OWNER TO musicbrainz;

--
-- Name: l_url_url_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_url_url_id_seq OWNED BY l_url_url.id;


--
-- Name: l_url_work; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_url_work (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_url_work_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_url_work OWNER TO musicbrainz;

--
-- Name: l_url_work_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_url_work_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_url_work_id_seq OWNER TO musicbrainz;

--
-- Name: l_url_work_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_url_work_id_seq OWNED BY l_url_work.id;


--
-- Name: l_work_work; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE l_work_work (
    id integer NOT NULL,
    link integer NOT NULL,
    entity0 integer NOT NULL,
    entity1 integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT l_work_work_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.l_work_work OWNER TO musicbrainz;

--
-- Name: l_work_work_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE l_work_work_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.l_work_work_id_seq OWNER TO musicbrainz;

--
-- Name: l_work_work_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE l_work_work_id_seq OWNED BY l_work_work.id;


--
-- Name: label; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE label (
    id integer NOT NULL,
    gid uuid NOT NULL,
    name character varying NOT NULL,
    sort_name character varying NOT NULL,
    begin_date_year smallint,
    begin_date_month smallint,
    begin_date_day smallint,
    end_date_year smallint,
    end_date_month smallint,
    end_date_day smallint,
    label_code integer,
    type integer,
    area integer,
    comment character varying(255) DEFAULT ''::character varying NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    ended boolean DEFAULT false NOT NULL,
    CONSTRAINT control_for_whitespace CHECK (controlled_for_whitespace((name)::text)),
    CONSTRAINT label_comment_check CHECK (controlled_for_whitespace((comment)::text)),
    CONSTRAINT label_edits_pending_check CHECK ((edits_pending >= 0)),
    CONSTRAINT label_ended_check CHECK ((((((end_date_year IS NOT NULL) OR (end_date_month IS NOT NULL)) OR (end_date_day IS NOT NULL)) AND (ended = true)) OR (((end_date_year IS NULL) AND (end_date_month IS NULL)) AND (end_date_day IS NULL)))),
    CONSTRAINT label_label_code_check CHECK (((label_code > 0) AND (label_code < 100000))),
    CONSTRAINT only_non_empty CHECK (((name)::text <> ''::text))
);


ALTER TABLE musicbrainz.label OWNER TO musicbrainz;

--
-- Name: label_alias; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE label_alias (
    id integer NOT NULL,
    label integer NOT NULL,
    name character varying NOT NULL,
    locale text,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    type integer,
    sort_name character varying NOT NULL,
    begin_date_year smallint,
    begin_date_month smallint,
    begin_date_day smallint,
    end_date_year smallint,
    end_date_month smallint,
    end_date_day smallint,
    primary_for_locale boolean DEFAULT false NOT NULL,
    ended boolean DEFAULT false NOT NULL,
    CONSTRAINT control_for_whitespace CHECK (controlled_for_whitespace((name)::text)),
    CONSTRAINT control_for_whitespace_sort_name CHECK (controlled_for_whitespace((sort_name)::text)),
    CONSTRAINT label_alias_edits_pending_check CHECK ((edits_pending >= 0)),
    CONSTRAINT label_alias_ended_check CHECK ((((((end_date_year IS NOT NULL) OR (end_date_month IS NOT NULL)) OR (end_date_day IS NOT NULL)) AND (ended = true)) OR (((end_date_year IS NULL) AND (end_date_month IS NULL)) AND (end_date_day IS NULL)))),
    CONSTRAINT only_non_empty CHECK (((name)::text <> ''::text)),
    CONSTRAINT only_non_empty_sort_name CHECK (((sort_name)::text <> ''::text)),
    CONSTRAINT primary_check CHECK ((((locale IS NULL) AND (primary_for_locale IS FALSE)) OR (locale IS NOT NULL))),
    CONSTRAINT search_hints_are_empty CHECK (((type <> 2) OR ((((((((((type = 2) AND ((sort_name)::text = (name)::text)) AND (begin_date_year IS NULL)) AND (begin_date_month IS NULL)) AND (begin_date_day IS NULL)) AND (end_date_year IS NULL)) AND (end_date_month IS NULL)) AND (end_date_day IS NULL)) AND (primary_for_locale IS FALSE)) AND (locale IS NULL))))
);


ALTER TABLE musicbrainz.label_alias OWNER TO musicbrainz;

--
-- Name: label_alias_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE label_alias_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.label_alias_id_seq OWNER TO musicbrainz;

--
-- Name: label_alias_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE label_alias_id_seq OWNED BY label_alias.id;


--
-- Name: label_alias_type; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE label_alias_type (
    id integer NOT NULL,
    name text NOT NULL
);


ALTER TABLE musicbrainz.label_alias_type OWNER TO musicbrainz;

--
-- Name: label_alias_type_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE label_alias_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.label_alias_type_id_seq OWNER TO musicbrainz;

--
-- Name: label_alias_type_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE label_alias_type_id_seq OWNED BY label_alias_type.id;


--
-- Name: label_annotation; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE label_annotation (
    label integer NOT NULL,
    annotation integer NOT NULL
);


ALTER TABLE musicbrainz.label_annotation OWNER TO musicbrainz;

--
-- Name: label_deletion; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE label_deletion (
    gid uuid NOT NULL,
    last_known_name character varying NOT NULL,
    last_known_comment text NOT NULL,
    deleted_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE musicbrainz.label_deletion OWNER TO musicbrainz;

--
-- Name: label_gid_redirect; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE label_gid_redirect (
    gid uuid NOT NULL,
    new_id integer NOT NULL,
    created timestamp with time zone DEFAULT now()
);


ALTER TABLE musicbrainz.label_gid_redirect OWNER TO musicbrainz;

--
-- Name: label_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE label_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.label_id_seq OWNER TO musicbrainz;

--
-- Name: label_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE label_id_seq OWNED BY label.id;


--
-- Name: label_ipi; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE label_ipi (
    label integer NOT NULL,
    ipi character(11) NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    created timestamp with time zone DEFAULT now(),
    CONSTRAINT label_ipi_edits_pending_check CHECK ((edits_pending >= 0)),
    CONSTRAINT label_ipi_ipi_check CHECK ((ipi ~ '^\d{11}$'::text))
);


ALTER TABLE musicbrainz.label_ipi OWNER TO musicbrainz;

--
-- Name: label_isni; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE label_isni (
    label integer NOT NULL,
    isni character(16) NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    created timestamp with time zone DEFAULT now(),
    CONSTRAINT label_isni_edits_pending_check CHECK ((edits_pending >= 0)),
    CONSTRAINT label_isni_isni_check CHECK ((isni ~ '^\d{15}[\dX]$'::text))
);


ALTER TABLE musicbrainz.label_isni OWNER TO musicbrainz;

--
-- Name: label_meta; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE label_meta (
    id integer NOT NULL,
    rating smallint,
    rating_count integer,
    CONSTRAINT label_meta_rating_check CHECK (((rating >= 0) AND (rating <= 100)))
);


ALTER TABLE musicbrainz.label_meta OWNER TO musicbrainz;

--
-- Name: label_rating_raw; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE label_rating_raw (
    label integer NOT NULL,
    editor integer NOT NULL,
    rating smallint NOT NULL,
    CONSTRAINT label_rating_raw_rating_check CHECK (((rating >= 0) AND (rating <= 100)))
);


ALTER TABLE musicbrainz.label_rating_raw OWNER TO musicbrainz;

--
-- Name: label_tag; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE label_tag (
    label integer NOT NULL,
    tag integer NOT NULL,
    count integer NOT NULL,
    last_updated timestamp with time zone DEFAULT now()
);


ALTER TABLE musicbrainz.label_tag OWNER TO musicbrainz;

--
-- Name: label_tag_raw; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE label_tag_raw (
    label integer NOT NULL,
    editor integer NOT NULL,
    tag integer NOT NULL
);


ALTER TABLE musicbrainz.label_tag_raw OWNER TO musicbrainz;

--
-- Name: label_type; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE label_type (
    id integer NOT NULL,
    name character varying(255) NOT NULL
);


ALTER TABLE musicbrainz.label_type OWNER TO musicbrainz;

--
-- Name: label_type_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE label_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.label_type_id_seq OWNER TO musicbrainz;

--
-- Name: label_type_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE label_type_id_seq OWNED BY label_type.id;


--
-- Name: language; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE language (
    id integer NOT NULL,
    iso_code_2t character(3),
    iso_code_2b character(3),
    iso_code_1 character(2),
    name character varying(100) NOT NULL,
    frequency integer DEFAULT 0 NOT NULL,
    iso_code_3 character(3),
    CONSTRAINT iso_code_check CHECK (((iso_code_2t IS NOT NULL) OR (iso_code_3 IS NOT NULL)))
);


ALTER TABLE musicbrainz.language OWNER TO musicbrainz;

--
-- Name: language_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE language_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.language_id_seq OWNER TO musicbrainz;

--
-- Name: language_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE language_id_seq OWNED BY language.id;


--
-- Name: link; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE link (
    id integer NOT NULL,
    link_type integer NOT NULL,
    begin_date_year smallint,
    begin_date_month smallint,
    begin_date_day smallint,
    end_date_year smallint,
    end_date_month smallint,
    end_date_day smallint,
    attribute_count integer DEFAULT 0 NOT NULL,
    created timestamp with time zone DEFAULT now(),
    ended boolean DEFAULT false NOT NULL,
    CONSTRAINT link_ended_check CHECK ((((((end_date_year IS NOT NULL) OR (end_date_month IS NOT NULL)) OR (end_date_day IS NOT NULL)) AND (ended = true)) OR (((end_date_year IS NULL) AND (end_date_month IS NULL)) AND (end_date_day IS NULL))))
);


ALTER TABLE musicbrainz.link OWNER TO musicbrainz;

--
-- Name: link_attribute; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE link_attribute (
    link integer NOT NULL,
    attribute_type integer NOT NULL,
    created timestamp with time zone DEFAULT now()
);


ALTER TABLE musicbrainz.link_attribute OWNER TO musicbrainz;

--
-- Name: link_attribute_credit; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE link_attribute_credit (
    link integer NOT NULL,
    attribute_type integer NOT NULL,
    credited_as text NOT NULL
);


ALTER TABLE musicbrainz.link_attribute_credit OWNER TO musicbrainz;

--
-- Name: link_attribute_type; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE link_attribute_type (
    id integer NOT NULL,
    parent integer,
    root integer NOT NULL,
    child_order integer DEFAULT 0 NOT NULL,
    gid uuid NOT NULL,
    name character varying(255) NOT NULL,
    description text,
    last_updated timestamp with time zone DEFAULT now()
);


ALTER TABLE musicbrainz.link_attribute_type OWNER TO musicbrainz;

--
-- Name: link_attribute_type_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE link_attribute_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.link_attribute_type_id_seq OWNER TO musicbrainz;

--
-- Name: link_attribute_type_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE link_attribute_type_id_seq OWNED BY link_attribute_type.id;


--
-- Name: link_creditable_attribute_type; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE link_creditable_attribute_type (
    attribute_type integer NOT NULL
);


ALTER TABLE musicbrainz.link_creditable_attribute_type OWNER TO musicbrainz;

--
-- Name: link_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE link_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.link_id_seq OWNER TO musicbrainz;

--
-- Name: link_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE link_id_seq OWNED BY link.id;


--
-- Name: link_type; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE link_type (
    id integer NOT NULL,
    parent integer,
    child_order integer DEFAULT 0 NOT NULL,
    gid uuid NOT NULL,
    entity_type0 character varying(50) NOT NULL,
    entity_type1 character varying(50) NOT NULL,
    name character varying(255) NOT NULL,
    description text,
    link_phrase character varying(255) NOT NULL,
    reverse_link_phrase character varying(255) NOT NULL,
    long_link_phrase character varying(255) NOT NULL,
    priority integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now()
);


ALTER TABLE musicbrainz.link_type OWNER TO musicbrainz;

--
-- Name: link_type_attribute_type; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE link_type_attribute_type (
    link_type integer NOT NULL,
    attribute_type integer NOT NULL,
    min smallint,
    max smallint,
    last_updated timestamp with time zone DEFAULT now()
);


ALTER TABLE musicbrainz.link_type_attribute_type OWNER TO musicbrainz;

--
-- Name: link_type_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE link_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.link_type_id_seq OWNER TO musicbrainz;

--
-- Name: link_type_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE link_type_id_seq OWNED BY link_type.id;


--
-- Name: medium; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE medium (
    id integer NOT NULL,
    release integer NOT NULL,
    "position" integer NOT NULL,
    format integer,
    name character varying(255),
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    track_count integer DEFAULT 0 NOT NULL,
    CONSTRAINT medium_edits_pending_check CHECK ((edits_pending >= 0)),
    CONSTRAINT medium_name_check CHECK (controlled_for_whitespace((name)::text))
);


ALTER TABLE musicbrainz.medium OWNER TO musicbrainz;

--
-- Name: medium_cdtoc; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE medium_cdtoc (
    id integer NOT NULL,
    medium integer NOT NULL,
    cdtoc integer NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT medium_cdtoc_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.medium_cdtoc OWNER TO musicbrainz;

--
-- Name: medium_cdtoc_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE medium_cdtoc_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.medium_cdtoc_id_seq OWNER TO musicbrainz;

--
-- Name: medium_cdtoc_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE medium_cdtoc_id_seq OWNED BY medium_cdtoc.id;


--
-- Name: medium_format; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE medium_format (
    id integer NOT NULL,
    name character varying(100) NOT NULL,
    parent integer,
    child_order integer DEFAULT 0 NOT NULL,
    year smallint,
    has_discids boolean DEFAULT false NOT NULL
);


ALTER TABLE musicbrainz.medium_format OWNER TO musicbrainz;

--
-- Name: medium_format_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE medium_format_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.medium_format_id_seq OWNER TO musicbrainz;

--
-- Name: medium_format_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE medium_format_id_seq OWNED BY medium_format.id;


--
-- Name: medium_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE medium_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.medium_id_seq OWNER TO musicbrainz;

--
-- Name: medium_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE medium_id_seq OWNED BY medium.id;


--
-- Name: medium_index; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE medium_index (
    medium integer NOT NULL,
    toc public.cube
);


ALTER TABLE musicbrainz.medium_index OWNER TO musicbrainz;

--
-- Name: recording; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE recording (
    id integer NOT NULL,
    gid uuid NOT NULL,
    name character varying NOT NULL,
    artist_credit integer NOT NULL,
    length integer,
    comment character varying(255) DEFAULT ''::character varying NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT control_for_whitespace CHECK (controlled_for_whitespace((name)::text)),
    CONSTRAINT only_non_empty CHECK (((name)::text <> ''::text)),
    CONSTRAINT recording_comment_check CHECK (controlled_for_whitespace((comment)::text)),
    CONSTRAINT recording_edits_pending_check CHECK ((edits_pending >= 0)),
    CONSTRAINT recording_length_check CHECK (((length IS NULL) OR (length > 0)))
);


ALTER TABLE musicbrainz.recording OWNER TO musicbrainz;

--
-- Name: recording_annotation; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE recording_annotation (
    recording integer NOT NULL,
    annotation integer NOT NULL
);


ALTER TABLE musicbrainz.recording_annotation OWNER TO musicbrainz;

--
-- Name: recording_gid_redirect; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE recording_gid_redirect (
    gid uuid NOT NULL,
    new_id integer NOT NULL,
    created timestamp with time zone DEFAULT now()
);


ALTER TABLE musicbrainz.recording_gid_redirect OWNER TO musicbrainz;

--
-- Name: recording_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE recording_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.recording_id_seq OWNER TO musicbrainz;

--
-- Name: recording_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE recording_id_seq OWNED BY recording.id;


--
-- Name: recording_meta; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE recording_meta (
    id integer NOT NULL,
    rating smallint,
    rating_count integer,
    CONSTRAINT recording_meta_rating_check CHECK (((rating >= 0) AND (rating <= 100)))
);


ALTER TABLE musicbrainz.recording_meta OWNER TO musicbrainz;

--
-- Name: recording_rating_raw; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE recording_rating_raw (
    recording integer NOT NULL,
    editor integer NOT NULL,
    rating smallint NOT NULL,
    CONSTRAINT recording_rating_raw_rating_check CHECK (((rating >= 0) AND (rating <= 100)))
);


ALTER TABLE musicbrainz.recording_rating_raw OWNER TO musicbrainz;

--
-- Name: recording_tag; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE recording_tag (
    recording integer NOT NULL,
    tag integer NOT NULL,
    count integer NOT NULL,
    last_updated timestamp with time zone DEFAULT now()
);


ALTER TABLE musicbrainz.recording_tag OWNER TO musicbrainz;

--
-- Name: recording_tag_raw; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE recording_tag_raw (
    recording integer NOT NULL,
    editor integer NOT NULL,
    tag integer NOT NULL
);


ALTER TABLE musicbrainz.recording_tag_raw OWNER TO musicbrainz;

--
-- Name: release; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release (
    id integer NOT NULL,
    gid uuid NOT NULL,
    name character varying NOT NULL,
    artist_credit integer NOT NULL,
    release_group integer NOT NULL,
    status integer,
    packaging integer,
    language integer,
    script integer,
    barcode character varying(255),
    comment character varying(255) DEFAULT ''::character varying NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    quality smallint DEFAULT (-1) NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT control_for_whitespace CHECK (controlled_for_whitespace((name)::text)),
    CONSTRAINT only_non_empty CHECK (((name)::text <> ''::text)),
    CONSTRAINT release_comment_check CHECK (controlled_for_whitespace((comment)::text)),
    CONSTRAINT release_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.release OWNER TO musicbrainz;

--
-- Name: release_annotation; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_annotation (
    release integer NOT NULL,
    annotation integer NOT NULL
);


ALTER TABLE musicbrainz.release_annotation OWNER TO musicbrainz;

--
-- Name: release_country; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_country (
    release integer NOT NULL,
    country integer NOT NULL,
    date_year smallint,
    date_month smallint,
    date_day smallint
);


ALTER TABLE musicbrainz.release_country OWNER TO musicbrainz;

--
-- Name: release_coverart; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_coverart (
    id integer NOT NULL,
    last_updated timestamp with time zone,
    cover_art_url character varying(255)
);


ALTER TABLE musicbrainz.release_coverart OWNER TO musicbrainz;

--
-- Name: release_gid_redirect; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_gid_redirect (
    gid uuid NOT NULL,
    new_id integer NOT NULL,
    created timestamp with time zone DEFAULT now()
);


ALTER TABLE musicbrainz.release_gid_redirect OWNER TO musicbrainz;

--
-- Name: release_group; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_group (
    id integer NOT NULL,
    gid uuid NOT NULL,
    name character varying NOT NULL,
    artist_credit integer NOT NULL,
    type integer,
    comment character varying(255) DEFAULT ''::character varying NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT control_for_whitespace CHECK (controlled_for_whitespace((name)::text)),
    CONSTRAINT only_non_empty CHECK (((name)::text <> ''::text)),
    CONSTRAINT release_group_comment_check CHECK (controlled_for_whitespace((comment)::text)),
    CONSTRAINT release_group_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.release_group OWNER TO musicbrainz;

--
-- Name: release_group_annotation; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_group_annotation (
    release_group integer NOT NULL,
    annotation integer NOT NULL
);


ALTER TABLE musicbrainz.release_group_annotation OWNER TO musicbrainz;

--
-- Name: release_group_gid_redirect; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_group_gid_redirect (
    gid uuid NOT NULL,
    new_id integer NOT NULL,
    created timestamp with time zone DEFAULT now()
);


ALTER TABLE musicbrainz.release_group_gid_redirect OWNER TO musicbrainz;

--
-- Name: release_group_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE release_group_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.release_group_id_seq OWNER TO musicbrainz;

--
-- Name: release_group_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE release_group_id_seq OWNED BY release_group.id;


--
-- Name: release_group_meta; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_group_meta (
    id integer NOT NULL,
    release_count integer DEFAULT 0 NOT NULL,
    first_release_date_year smallint,
    first_release_date_month smallint,
    first_release_date_day smallint,
    rating smallint,
    rating_count integer,
    CONSTRAINT release_group_meta_rating_check CHECK (((rating >= 0) AND (rating <= 100)))
);


ALTER TABLE musicbrainz.release_group_meta OWNER TO musicbrainz;

--
-- Name: release_group_primary_type; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_group_primary_type (
    id integer NOT NULL,
    name character varying(255) NOT NULL
);


ALTER TABLE musicbrainz.release_group_primary_type OWNER TO musicbrainz;

--
-- Name: release_group_primary_type_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE release_group_primary_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.release_group_primary_type_id_seq OWNER TO musicbrainz;

--
-- Name: release_group_primary_type_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE release_group_primary_type_id_seq OWNED BY release_group_primary_type.id;


--
-- Name: release_group_rating_raw; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_group_rating_raw (
    release_group integer NOT NULL,
    editor integer NOT NULL,
    rating smallint NOT NULL,
    CONSTRAINT release_group_rating_raw_rating_check CHECK (((rating >= 0) AND (rating <= 100)))
);


ALTER TABLE musicbrainz.release_group_rating_raw OWNER TO musicbrainz;

--
-- Name: release_group_secondary_type; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_group_secondary_type (
    id integer NOT NULL,
    name text NOT NULL
);


ALTER TABLE musicbrainz.release_group_secondary_type OWNER TO musicbrainz;

--
-- Name: release_group_secondary_type_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE release_group_secondary_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.release_group_secondary_type_id_seq OWNER TO musicbrainz;

--
-- Name: release_group_secondary_type_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE release_group_secondary_type_id_seq OWNED BY release_group_secondary_type.id;


--
-- Name: release_group_secondary_type_join; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_group_secondary_type_join (
    release_group integer NOT NULL,
    secondary_type integer NOT NULL,
    created timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE musicbrainz.release_group_secondary_type_join OWNER TO musicbrainz;

--
-- Name: release_group_tag; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_group_tag (
    release_group integer NOT NULL,
    tag integer NOT NULL,
    count integer NOT NULL,
    last_updated timestamp with time zone DEFAULT now()
);


ALTER TABLE musicbrainz.release_group_tag OWNER TO musicbrainz;

--
-- Name: release_group_tag_raw; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_group_tag_raw (
    release_group integer NOT NULL,
    editor integer NOT NULL,
    tag integer NOT NULL
);


ALTER TABLE musicbrainz.release_group_tag_raw OWNER TO musicbrainz;

--
-- Name: release_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE release_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.release_id_seq OWNER TO musicbrainz;

--
-- Name: release_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE release_id_seq OWNED BY release.id;


--
-- Name: release_label; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_label (
    id integer NOT NULL,
    release integer NOT NULL,
    label integer,
    catalog_number character varying(255),
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT release_label_catalog_number_check CHECK (controlled_for_whitespace((catalog_number)::text)),
    CONSTRAINT release_label_check CHECK (((catalog_number IS NOT NULL) OR (label IS NOT NULL)))
);


ALTER TABLE musicbrainz.release_label OWNER TO musicbrainz;

--
-- Name: release_label_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE release_label_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.release_label_id_seq OWNER TO musicbrainz;

--
-- Name: release_label_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE release_label_id_seq OWNED BY release_label.id;


--
-- Name: release_meta; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_meta (
    id integer NOT NULL,
    date_added timestamp with time zone DEFAULT now(),
    info_url character varying(255),
    amazon_asin character varying(10),
    amazon_store character varying(20),
    cover_art_presence cover_art_presence DEFAULT 'absent'::cover_art_presence NOT NULL
);


ALTER TABLE musicbrainz.release_meta OWNER TO musicbrainz;

--
-- Name: release_packaging; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_packaging (
    id integer NOT NULL,
    name character varying(255) NOT NULL
);


ALTER TABLE musicbrainz.release_packaging OWNER TO musicbrainz;

--
-- Name: release_packaging_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE release_packaging_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.release_packaging_id_seq OWNER TO musicbrainz;

--
-- Name: release_packaging_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE release_packaging_id_seq OWNED BY release_packaging.id;


--
-- Name: release_raw; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_raw (
    id integer NOT NULL,
    title character varying(255) NOT NULL,
    artist character varying(255),
    added timestamp with time zone DEFAULT now(),
    last_modified timestamp with time zone DEFAULT now(),
    lookup_count integer DEFAULT 0,
    modify_count integer DEFAULT 0,
    source integer DEFAULT 0,
    barcode character varying(255),
    comment character varying(255) DEFAULT ''::character varying NOT NULL
);


ALTER TABLE musicbrainz.release_raw OWNER TO musicbrainz;

--
-- Name: release_raw_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE release_raw_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.release_raw_id_seq OWNER TO musicbrainz;

--
-- Name: release_raw_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE release_raw_id_seq OWNED BY release_raw.id;


--
-- Name: release_status; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_status (
    id integer NOT NULL,
    name character varying(255) NOT NULL
);


ALTER TABLE musicbrainz.release_status OWNER TO musicbrainz;

--
-- Name: release_status_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE release_status_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.release_status_id_seq OWNER TO musicbrainz;

--
-- Name: release_status_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE release_status_id_seq OWNED BY release_status.id;


--
-- Name: release_tag; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_tag (
    release integer NOT NULL,
    tag integer NOT NULL,
    count integer NOT NULL,
    last_updated timestamp with time zone DEFAULT now()
);


ALTER TABLE musicbrainz.release_tag OWNER TO musicbrainz;

--
-- Name: release_tag_raw; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_tag_raw (
    release integer NOT NULL,
    editor integer NOT NULL,
    tag integer NOT NULL
);


ALTER TABLE musicbrainz.release_tag_raw OWNER TO musicbrainz;

--
-- Name: release_unknown_country; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE release_unknown_country (
    release integer NOT NULL,
    date_year smallint,
    date_month smallint,
    date_day smallint,
    CONSTRAINT non_empty_date CHECK ((((date_year IS NOT NULL) OR (date_month IS NOT NULL)) OR (date_day IS NOT NULL)))
);


ALTER TABLE musicbrainz.release_unknown_country OWNER TO musicbrainz;

--
-- Name: replication_control; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE replication_control (
    id integer NOT NULL,
    current_schema_sequence integer NOT NULL,
    current_replication_sequence integer,
    last_replication_date timestamp with time zone
);


ALTER TABLE musicbrainz.replication_control OWNER TO musicbrainz;

--
-- Name: replication_control_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE replication_control_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.replication_control_id_seq OWNER TO musicbrainz;

--
-- Name: replication_control_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE replication_control_id_seq OWNED BY replication_control.id;


--
-- Name: script; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE script (
    id integer NOT NULL,
    iso_code character(4) NOT NULL,
    iso_number character(3) NOT NULL,
    name character varying(100) NOT NULL,
    frequency integer DEFAULT 0 NOT NULL
);


ALTER TABLE musicbrainz.script OWNER TO musicbrainz;

--
-- Name: script_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE script_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.script_id_seq OWNER TO musicbrainz;

--
-- Name: script_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE script_id_seq OWNED BY script.id;


--
-- Name: script_language; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE script_language (
    id integer NOT NULL,
    script integer NOT NULL,
    language integer NOT NULL,
    frequency integer DEFAULT 0 NOT NULL
);


ALTER TABLE musicbrainz.script_language OWNER TO musicbrainz;

--
-- Name: script_language_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE script_language_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.script_language_id_seq OWNER TO musicbrainz;

--
-- Name: script_language_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE script_language_id_seq OWNED BY script_language.id;


--
-- Name: tag; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE tag (
    id integer NOT NULL,
    name character varying(255) NOT NULL,
    ref_count integer DEFAULT 0 NOT NULL
);


ALTER TABLE musicbrainz.tag OWNER TO musicbrainz;

--
-- Name: tag_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE tag_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.tag_id_seq OWNER TO musicbrainz;

--
-- Name: tag_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE tag_id_seq OWNED BY tag.id;


--
-- Name: tag_relation; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE tag_relation (
    tag1 integer NOT NULL,
    tag2 integer NOT NULL,
    weight integer NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT tag_relation_check CHECK ((tag1 < tag2))
);


ALTER TABLE musicbrainz.tag_relation OWNER TO musicbrainz;

--
-- Name: track; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE track (
    id integer NOT NULL,
    gid uuid NOT NULL,
    recording integer NOT NULL,
    medium integer NOT NULL,
    "position" integer NOT NULL,
    number text NOT NULL,
    name character varying NOT NULL,
    artist_credit integer NOT NULL,
    length integer,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT control_for_whitespace CHECK (controlled_for_whitespace((name)::text)),
    CONSTRAINT only_non_empty CHECK (((name)::text <> ''::text)),
    CONSTRAINT track_edits_pending_check CHECK ((edits_pending >= 0)),
    CONSTRAINT track_length_check CHECK (((length IS NULL) OR (length > 0))),
    CONSTRAINT track_number_check CHECK (controlled_for_whitespace(number))
);


ALTER TABLE musicbrainz.track OWNER TO musicbrainz;

--
-- Name: track_gid_redirect; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE track_gid_redirect (
    gid uuid NOT NULL,
    new_id integer NOT NULL,
    created timestamp with time zone DEFAULT now()
);


ALTER TABLE musicbrainz.track_gid_redirect OWNER TO musicbrainz;

--
-- Name: track_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE track_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.track_id_seq OWNER TO musicbrainz;

--
-- Name: track_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE track_id_seq OWNED BY track.id;


--
-- Name: track_raw; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE track_raw (
    id integer NOT NULL,
    release integer NOT NULL,
    title character varying(255) NOT NULL,
    artist character varying(255),
    sequence integer NOT NULL
);


ALTER TABLE musicbrainz.track_raw OWNER TO musicbrainz;

--
-- Name: track_raw_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE track_raw_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.track_raw_id_seq OWNER TO musicbrainz;

--
-- Name: track_raw_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE track_raw_id_seq OWNED BY track_raw.id;


--
-- Name: url; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE url (
    id integer NOT NULL,
    gid uuid NOT NULL,
    url text NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    CONSTRAINT url_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.url OWNER TO musicbrainz;

--
-- Name: url_gid_redirect; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE url_gid_redirect (
    gid uuid NOT NULL,
    new_id integer NOT NULL,
    created timestamp with time zone DEFAULT now()
);


ALTER TABLE musicbrainz.url_gid_redirect OWNER TO musicbrainz;

--
-- Name: url_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE url_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.url_id_seq OWNER TO musicbrainz;

--
-- Name: url_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE url_id_seq OWNED BY url.id;


--
-- Name: vote; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE vote (
    id integer NOT NULL,
    editor integer NOT NULL,
    edit integer NOT NULL,
    vote smallint NOT NULL,
    vote_time timestamp with time zone DEFAULT now(),
    superseded boolean DEFAULT false NOT NULL
);


ALTER TABLE musicbrainz.vote OWNER TO musicbrainz;

--
-- Name: vote_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE vote_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.vote_id_seq OWNER TO musicbrainz;

--
-- Name: vote_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE vote_id_seq OWNED BY vote.id;


--
-- Name: work; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE work (
    id integer NOT NULL,
    gid uuid NOT NULL,
    name character varying NOT NULL,
    type integer,
    comment character varying(255) DEFAULT ''::character varying NOT NULL,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    language integer,
    CONSTRAINT control_for_whitespace CHECK (controlled_for_whitespace((name)::text)),
    CONSTRAINT only_non_empty CHECK (((name)::text <> ''::text)),
    CONSTRAINT work_comment_check CHECK (controlled_for_whitespace((comment)::text)),
    CONSTRAINT work_edits_pending_check CHECK ((edits_pending >= 0))
);


ALTER TABLE musicbrainz.work OWNER TO musicbrainz;

--
-- Name: work_alias; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE work_alias (
    id integer NOT NULL,
    work integer NOT NULL,
    name character varying NOT NULL,
    locale text,
    edits_pending integer DEFAULT 0 NOT NULL,
    last_updated timestamp with time zone DEFAULT now(),
    type integer,
    sort_name character varying NOT NULL,
    begin_date_year smallint,
    begin_date_month smallint,
    begin_date_day smallint,
    end_date_year smallint,
    end_date_month smallint,
    end_date_day smallint,
    primary_for_locale boolean DEFAULT false NOT NULL,
    ended boolean DEFAULT false NOT NULL,
    CONSTRAINT control_for_whitespace CHECK (controlled_for_whitespace((name)::text)),
    CONSTRAINT control_for_whitespace_sort_name CHECK (controlled_for_whitespace((sort_name)::text)),
    CONSTRAINT only_non_empty CHECK (((name)::text <> ''::text)),
    CONSTRAINT only_non_empty_sort_name CHECK (((sort_name)::text <> ''::text)),
    CONSTRAINT primary_check CHECK ((((locale IS NULL) AND (primary_for_locale IS FALSE)) OR (locale IS NOT NULL))),
    CONSTRAINT search_hints_are_empty CHECK (((type <> 2) OR ((((((((((type = 2) AND ((sort_name)::text = (name)::text)) AND (begin_date_year IS NULL)) AND (begin_date_month IS NULL)) AND (begin_date_day IS NULL)) AND (end_date_year IS NULL)) AND (end_date_month IS NULL)) AND (end_date_day IS NULL)) AND (primary_for_locale IS FALSE)) AND (locale IS NULL)))),
    CONSTRAINT work_alias_edits_pending_check CHECK ((edits_pending >= 0)),
    CONSTRAINT work_alias_ended_check CHECK ((((((end_date_year IS NOT NULL) OR (end_date_month IS NOT NULL)) OR (end_date_day IS NOT NULL)) AND (ended = true)) OR (((end_date_year IS NULL) AND (end_date_month IS NULL)) AND (end_date_day IS NULL))))
);


ALTER TABLE musicbrainz.work_alias OWNER TO musicbrainz;

--
-- Name: work_alias_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE work_alias_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.work_alias_id_seq OWNER TO musicbrainz;

--
-- Name: work_alias_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE work_alias_id_seq OWNED BY work_alias.id;


--
-- Name: work_alias_type; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE work_alias_type (
    id integer NOT NULL,
    name text NOT NULL
);


ALTER TABLE musicbrainz.work_alias_type OWNER TO musicbrainz;

--
-- Name: work_alias_type_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE work_alias_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.work_alias_type_id_seq OWNER TO musicbrainz;

--
-- Name: work_alias_type_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE work_alias_type_id_seq OWNED BY work_alias_type.id;


--
-- Name: work_annotation; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE work_annotation (
    work integer NOT NULL,
    annotation integer NOT NULL
);


ALTER TABLE musicbrainz.work_annotation OWNER TO musicbrainz;

--
-- Name: work_attribute; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE work_attribute (
    id integer NOT NULL,
    work integer NOT NULL,
    work_attribute_type integer NOT NULL,
    work_attribute_type_allowed_value integer,
    work_attribute_text text,
    CONSTRAINT work_attribute_check CHECK (((work_attribute_type_allowed_value IS NULL) OR (work_attribute_text IS NULL)))
);


ALTER TABLE musicbrainz.work_attribute OWNER TO musicbrainz;

--
-- Name: work_attribute_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE work_attribute_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.work_attribute_id_seq OWNER TO musicbrainz;

--
-- Name: work_attribute_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE work_attribute_id_seq OWNED BY work_attribute.id;


--
-- Name: work_attribute_type; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE work_attribute_type (
    id integer NOT NULL,
    name character varying(255) NOT NULL,
    comment character varying(255) DEFAULT ''::character varying NOT NULL,
    free_text boolean NOT NULL
);


ALTER TABLE musicbrainz.work_attribute_type OWNER TO musicbrainz;

--
-- Name: work_attribute_type_allowed_value; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE work_attribute_type_allowed_value (
    id integer NOT NULL,
    work_attribute_type integer NOT NULL,
    value text
);


ALTER TABLE musicbrainz.work_attribute_type_allowed_value OWNER TO musicbrainz;

--
-- Name: work_attribute_type_allowed_value_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE work_attribute_type_allowed_value_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.work_attribute_type_allowed_value_id_seq OWNER TO musicbrainz;

--
-- Name: work_attribute_type_allowed_value_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE work_attribute_type_allowed_value_id_seq OWNED BY work_attribute_type_allowed_value.id;


--
-- Name: work_attribute_type_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE work_attribute_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.work_attribute_type_id_seq OWNER TO musicbrainz;

--
-- Name: work_attribute_type_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE work_attribute_type_id_seq OWNED BY work_attribute_type.id;


--
-- Name: work_gid_redirect; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE work_gid_redirect (
    gid uuid NOT NULL,
    new_id integer NOT NULL,
    created timestamp with time zone DEFAULT now()
);


ALTER TABLE musicbrainz.work_gid_redirect OWNER TO musicbrainz;

--
-- Name: work_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE work_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.work_id_seq OWNER TO musicbrainz;

--
-- Name: work_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE work_id_seq OWNED BY work.id;


--
-- Name: work_meta; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE work_meta (
    id integer NOT NULL,
    rating smallint,
    rating_count integer,
    CONSTRAINT work_meta_rating_check CHECK (((rating >= 0) AND (rating <= 100)))
);


ALTER TABLE musicbrainz.work_meta OWNER TO musicbrainz;

--
-- Name: work_rating_raw; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE work_rating_raw (
    work integer NOT NULL,
    editor integer NOT NULL,
    rating smallint NOT NULL,
    CONSTRAINT work_rating_raw_rating_check CHECK (((rating >= 0) AND (rating <= 100)))
);


ALTER TABLE musicbrainz.work_rating_raw OWNER TO musicbrainz;

--
-- Name: work_tag; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE work_tag (
    work integer NOT NULL,
    tag integer NOT NULL,
    count integer NOT NULL,
    last_updated timestamp with time zone DEFAULT now()
);


ALTER TABLE musicbrainz.work_tag OWNER TO musicbrainz;

--
-- Name: work_tag_raw; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE work_tag_raw (
    work integer NOT NULL,
    editor integer NOT NULL,
    tag integer NOT NULL
);


ALTER TABLE musicbrainz.work_tag_raw OWNER TO musicbrainz;

--
-- Name: work_type; Type: TABLE; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE work_type (
    id integer NOT NULL,
    name character varying(255) NOT NULL
);


ALTER TABLE musicbrainz.work_type OWNER TO musicbrainz;

--
-- Name: work_type_id_seq; Type: SEQUENCE; Schema: musicbrainz; Owner: musicbrainz
--

CREATE SEQUENCE work_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE musicbrainz.work_type_id_seq OWNER TO musicbrainz;

--
-- Name: work_type_id_seq; Type: SEQUENCE OWNED BY; Schema: musicbrainz; Owner: musicbrainz
--

ALTER SEQUENCE work_type_id_seq OWNED BY work_type.id;


SET search_path = report, pg_catalog;

--
-- Name: index; Type: TABLE; Schema: report; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE index (
    report_name text NOT NULL,
    generated_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE report.index OWNER TO musicbrainz;

SET search_path = statistics, pg_catalog;

--
-- Name: log_statistic; Type: TABLE; Schema: statistics; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE log_statistic (
    name text NOT NULL,
    category text NOT NULL,
    "timestamp" timestamp with time zone DEFAULT now() NOT NULL,
    data text NOT NULL
);


ALTER TABLE statistics.log_statistic OWNER TO musicbrainz;

--
-- Name: statistic; Type: TABLE; Schema: statistics; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE statistic (
    id integer NOT NULL,
    name character varying(100) NOT NULL,
    value integer NOT NULL,
    date_collected date DEFAULT now() NOT NULL
);


ALTER TABLE statistics.statistic OWNER TO musicbrainz;

--
-- Name: statistic_event; Type: TABLE; Schema: statistics; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE statistic_event (
    date date NOT NULL,
    title text NOT NULL,
    link text NOT NULL,
    description text NOT NULL,
    CONSTRAINT statistic_event_date_check CHECK ((date >= '2000-01-01'::date))
);


ALTER TABLE statistics.statistic_event OWNER TO musicbrainz;

--
-- Name: statistic_id_seq; Type: SEQUENCE; Schema: statistics; Owner: musicbrainz
--

CREATE SEQUENCE statistic_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE statistics.statistic_id_seq OWNER TO musicbrainz;

--
-- Name: statistic_id_seq; Type: SEQUENCE OWNED BY; Schema: statistics; Owner: musicbrainz
--

ALTER SEQUENCE statistic_id_seq OWNED BY statistic.id;


SET search_path = wikidocs, pg_catalog;

--
-- Name: wikidocs_index; Type: TABLE; Schema: wikidocs; Owner: musicbrainz; Tablespace: 
--

CREATE TABLE wikidocs_index (
    page_name text NOT NULL,
    revision integer NOT NULL
);


ALTER TABLE wikidocs.wikidocs_index OWNER TO musicbrainz;

SET search_path = cover_art_archive, pg_catalog;

--
-- Name: id; Type: DEFAULT; Schema: cover_art_archive; Owner: musicbrainz
--

ALTER TABLE ONLY art_type ALTER COLUMN id SET DEFAULT nextval('art_type_id_seq'::regclass);


SET search_path = musicbrainz, pg_catalog;

--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY annotation ALTER COLUMN id SET DEFAULT nextval('annotation_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY application ALTER COLUMN id SET DEFAULT nextval('application_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY area ALTER COLUMN id SET DEFAULT nextval('area_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY area_alias ALTER COLUMN id SET DEFAULT nextval('area_alias_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY area_alias_type ALTER COLUMN id SET DEFAULT nextval('area_alias_type_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY area_type ALTER COLUMN id SET DEFAULT nextval('area_type_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist ALTER COLUMN id SET DEFAULT nextval('artist_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_alias ALTER COLUMN id SET DEFAULT nextval('artist_alias_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_alias_type ALTER COLUMN id SET DEFAULT nextval('artist_alias_type_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_credit ALTER COLUMN id SET DEFAULT nextval('artist_credit_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_type ALTER COLUMN id SET DEFAULT nextval('artist_type_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY autoeditor_election ALTER COLUMN id SET DEFAULT nextval('autoeditor_election_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY autoeditor_election_vote ALTER COLUMN id SET DEFAULT nextval('autoeditor_election_vote_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY cdtoc ALTER COLUMN id SET DEFAULT nextval('cdtoc_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY cdtoc_raw ALTER COLUMN id SET DEFAULT nextval('cdtoc_raw_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY edit ALTER COLUMN id SET DEFAULT nextval('edit_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY edit_note ALTER COLUMN id SET DEFAULT nextval('edit_note_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor ALTER COLUMN id SET DEFAULT nextval('editor_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_collection ALTER COLUMN id SET DEFAULT nextval('editor_collection_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_oauth_token ALTER COLUMN id SET DEFAULT nextval('editor_oauth_token_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_preference ALTER COLUMN id SET DEFAULT nextval('editor_preference_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_subscribe_artist ALTER COLUMN id SET DEFAULT nextval('editor_subscribe_artist_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_subscribe_collection ALTER COLUMN id SET DEFAULT nextval('editor_subscribe_collection_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_subscribe_editor ALTER COLUMN id SET DEFAULT nextval('editor_subscribe_editor_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_subscribe_label ALTER COLUMN id SET DEFAULT nextval('editor_subscribe_label_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY gender ALTER COLUMN id SET DEFAULT nextval('gender_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY isrc ALTER COLUMN id SET DEFAULT nextval('isrc_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY iswc ALTER COLUMN id SET DEFAULT nextval('iswc_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_area ALTER COLUMN id SET DEFAULT nextval('l_area_area_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_artist ALTER COLUMN id SET DEFAULT nextval('l_area_artist_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_label ALTER COLUMN id SET DEFAULT nextval('l_area_label_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_recording ALTER COLUMN id SET DEFAULT nextval('l_area_recording_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_release ALTER COLUMN id SET DEFAULT nextval('l_area_release_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_release_group ALTER COLUMN id SET DEFAULT nextval('l_area_release_group_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_url ALTER COLUMN id SET DEFAULT nextval('l_area_url_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_work ALTER COLUMN id SET DEFAULT nextval('l_area_work_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_artist_artist ALTER COLUMN id SET DEFAULT nextval('l_artist_artist_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_artist_label ALTER COLUMN id SET DEFAULT nextval('l_artist_label_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_artist_recording ALTER COLUMN id SET DEFAULT nextval('l_artist_recording_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_artist_release ALTER COLUMN id SET DEFAULT nextval('l_artist_release_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_artist_release_group ALTER COLUMN id SET DEFAULT nextval('l_artist_release_group_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_artist_url ALTER COLUMN id SET DEFAULT nextval('l_artist_url_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_artist_work ALTER COLUMN id SET DEFAULT nextval('l_artist_work_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_label_label ALTER COLUMN id SET DEFAULT nextval('l_label_label_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_label_recording ALTER COLUMN id SET DEFAULT nextval('l_label_recording_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_label_release ALTER COLUMN id SET DEFAULT nextval('l_label_release_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_label_release_group ALTER COLUMN id SET DEFAULT nextval('l_label_release_group_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_label_url ALTER COLUMN id SET DEFAULT nextval('l_label_url_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_label_work ALTER COLUMN id SET DEFAULT nextval('l_label_work_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_recording_recording ALTER COLUMN id SET DEFAULT nextval('l_recording_recording_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_recording_release ALTER COLUMN id SET DEFAULT nextval('l_recording_release_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_recording_release_group ALTER COLUMN id SET DEFAULT nextval('l_recording_release_group_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_recording_url ALTER COLUMN id SET DEFAULT nextval('l_recording_url_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_recording_work ALTER COLUMN id SET DEFAULT nextval('l_recording_work_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_release_group_release_group ALTER COLUMN id SET DEFAULT nextval('l_release_group_release_group_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_release_group_url ALTER COLUMN id SET DEFAULT nextval('l_release_group_url_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_release_group_work ALTER COLUMN id SET DEFAULT nextval('l_release_group_work_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_release_release ALTER COLUMN id SET DEFAULT nextval('l_release_release_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_release_release_group ALTER COLUMN id SET DEFAULT nextval('l_release_release_group_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_release_url ALTER COLUMN id SET DEFAULT nextval('l_release_url_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_release_work ALTER COLUMN id SET DEFAULT nextval('l_release_work_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_url_url ALTER COLUMN id SET DEFAULT nextval('l_url_url_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_url_work ALTER COLUMN id SET DEFAULT nextval('l_url_work_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_work_work ALTER COLUMN id SET DEFAULT nextval('l_work_work_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label ALTER COLUMN id SET DEFAULT nextval('label_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_alias ALTER COLUMN id SET DEFAULT nextval('label_alias_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_alias_type ALTER COLUMN id SET DEFAULT nextval('label_alias_type_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_type ALTER COLUMN id SET DEFAULT nextval('label_type_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY language ALTER COLUMN id SET DEFAULT nextval('language_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY link ALTER COLUMN id SET DEFAULT nextval('link_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY link_attribute_type ALTER COLUMN id SET DEFAULT nextval('link_attribute_type_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY link_type ALTER COLUMN id SET DEFAULT nextval('link_type_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY medium ALTER COLUMN id SET DEFAULT nextval('medium_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY medium_cdtoc ALTER COLUMN id SET DEFAULT nextval('medium_cdtoc_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY medium_format ALTER COLUMN id SET DEFAULT nextval('medium_format_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY recording ALTER COLUMN id SET DEFAULT nextval('recording_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release ALTER COLUMN id SET DEFAULT nextval('release_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group ALTER COLUMN id SET DEFAULT nextval('release_group_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group_primary_type ALTER COLUMN id SET DEFAULT nextval('release_group_primary_type_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group_secondary_type ALTER COLUMN id SET DEFAULT nextval('release_group_secondary_type_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_label ALTER COLUMN id SET DEFAULT nextval('release_label_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_packaging ALTER COLUMN id SET DEFAULT nextval('release_packaging_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_raw ALTER COLUMN id SET DEFAULT nextval('release_raw_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_status ALTER COLUMN id SET DEFAULT nextval('release_status_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY replication_control ALTER COLUMN id SET DEFAULT nextval('replication_control_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY script ALTER COLUMN id SET DEFAULT nextval('script_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY script_language ALTER COLUMN id SET DEFAULT nextval('script_language_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY tag ALTER COLUMN id SET DEFAULT nextval('tag_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY track ALTER COLUMN id SET DEFAULT nextval('track_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY track_raw ALTER COLUMN id SET DEFAULT nextval('track_raw_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY url ALTER COLUMN id SET DEFAULT nextval('url_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY vote ALTER COLUMN id SET DEFAULT nextval('vote_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work ALTER COLUMN id SET DEFAULT nextval('work_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_alias ALTER COLUMN id SET DEFAULT nextval('work_alias_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_alias_type ALTER COLUMN id SET DEFAULT nextval('work_alias_type_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_attribute ALTER COLUMN id SET DEFAULT nextval('work_attribute_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_attribute_type ALTER COLUMN id SET DEFAULT nextval('work_attribute_type_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_attribute_type_allowed_value ALTER COLUMN id SET DEFAULT nextval('work_attribute_type_allowed_value_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_type ALTER COLUMN id SET DEFAULT nextval('work_type_id_seq'::regclass);


SET search_path = statistics, pg_catalog;

--
-- Name: id; Type: DEFAULT; Schema: statistics; Owner: musicbrainz
--

ALTER TABLE ONLY statistic ALTER COLUMN id SET DEFAULT nextval('statistic_id_seq'::regclass);


SET search_path = cover_art_archive, pg_catalog;

--
-- Name: art_type_pkey; Type: CONSTRAINT; Schema: cover_art_archive; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY art_type
    ADD CONSTRAINT art_type_pkey PRIMARY KEY (id);


--
-- Name: cover_art_pkey; Type: CONSTRAINT; Schema: cover_art_archive; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY cover_art
    ADD CONSTRAINT cover_art_pkey PRIMARY KEY (id);


--
-- Name: cover_art_type_pkey; Type: CONSTRAINT; Schema: cover_art_archive; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY cover_art_type
    ADD CONSTRAINT cover_art_type_pkey PRIMARY KEY (id, type_id);


--
-- Name: image_type_pkey; Type: CONSTRAINT; Schema: cover_art_archive; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY image_type
    ADD CONSTRAINT image_type_pkey PRIMARY KEY (mime_type);


--
-- Name: release_group_cover_art_pkey; Type: CONSTRAINT; Schema: cover_art_archive; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_group_cover_art
    ADD CONSTRAINT release_group_cover_art_pkey PRIMARY KEY (release_group);


SET search_path = documentation, pg_catalog;

--
-- Name: l_area_area_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_area_area_example
    ADD CONSTRAINT l_area_area_example_pkey PRIMARY KEY (id);


--
-- Name: l_area_artist_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_area_artist_example
    ADD CONSTRAINT l_area_artist_example_pkey PRIMARY KEY (id);


--
-- Name: l_area_label_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_area_label_example
    ADD CONSTRAINT l_area_label_example_pkey PRIMARY KEY (id);


--
-- Name: l_area_recording_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_area_recording_example
    ADD CONSTRAINT l_area_recording_example_pkey PRIMARY KEY (id);


--
-- Name: l_area_release_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_area_release_example
    ADD CONSTRAINT l_area_release_example_pkey PRIMARY KEY (id);


--
-- Name: l_area_release_group_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_area_release_group_example
    ADD CONSTRAINT l_area_release_group_example_pkey PRIMARY KEY (id);


--
-- Name: l_area_url_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_area_url_example
    ADD CONSTRAINT l_area_url_example_pkey PRIMARY KEY (id);


--
-- Name: l_area_work_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_area_work_example
    ADD CONSTRAINT l_area_work_example_pkey PRIMARY KEY (id);


--
-- Name: l_artist_artist_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_artist_artist_example
    ADD CONSTRAINT l_artist_artist_example_pkey PRIMARY KEY (id);


--
-- Name: l_artist_label_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_artist_label_example
    ADD CONSTRAINT l_artist_label_example_pkey PRIMARY KEY (id);


--
-- Name: l_artist_recording_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_artist_recording_example
    ADD CONSTRAINT l_artist_recording_example_pkey PRIMARY KEY (id);


--
-- Name: l_artist_release_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_artist_release_example
    ADD CONSTRAINT l_artist_release_example_pkey PRIMARY KEY (id);


--
-- Name: l_artist_release_group_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_artist_release_group_example
    ADD CONSTRAINT l_artist_release_group_example_pkey PRIMARY KEY (id);


--
-- Name: l_artist_url_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_artist_url_example
    ADD CONSTRAINT l_artist_url_example_pkey PRIMARY KEY (id);


--
-- Name: l_artist_work_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_artist_work_example
    ADD CONSTRAINT l_artist_work_example_pkey PRIMARY KEY (id);


--
-- Name: l_label_label_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_label_label_example
    ADD CONSTRAINT l_label_label_example_pkey PRIMARY KEY (id);


--
-- Name: l_label_recording_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_label_recording_example
    ADD CONSTRAINT l_label_recording_example_pkey PRIMARY KEY (id);


--
-- Name: l_label_release_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_label_release_example
    ADD CONSTRAINT l_label_release_example_pkey PRIMARY KEY (id);


--
-- Name: l_label_release_group_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_label_release_group_example
    ADD CONSTRAINT l_label_release_group_example_pkey PRIMARY KEY (id);


--
-- Name: l_label_url_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_label_url_example
    ADD CONSTRAINT l_label_url_example_pkey PRIMARY KEY (id);


--
-- Name: l_label_work_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_label_work_example
    ADD CONSTRAINT l_label_work_example_pkey PRIMARY KEY (id);


--
-- Name: l_recording_recording_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_recording_recording_example
    ADD CONSTRAINT l_recording_recording_example_pkey PRIMARY KEY (id);


--
-- Name: l_recording_release_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_recording_release_example
    ADD CONSTRAINT l_recording_release_example_pkey PRIMARY KEY (id);


--
-- Name: l_recording_release_group_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_recording_release_group_example
    ADD CONSTRAINT l_recording_release_group_example_pkey PRIMARY KEY (id);


--
-- Name: l_recording_url_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_recording_url_example
    ADD CONSTRAINT l_recording_url_example_pkey PRIMARY KEY (id);


--
-- Name: l_recording_work_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_recording_work_example
    ADD CONSTRAINT l_recording_work_example_pkey PRIMARY KEY (id);


--
-- Name: l_release_group_release_group_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_release_group_release_group_example
    ADD CONSTRAINT l_release_group_release_group_example_pkey PRIMARY KEY (id);


--
-- Name: l_release_group_url_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_release_group_url_example
    ADD CONSTRAINT l_release_group_url_example_pkey PRIMARY KEY (id);


--
-- Name: l_release_group_work_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_release_group_work_example
    ADD CONSTRAINT l_release_group_work_example_pkey PRIMARY KEY (id);


--
-- Name: l_release_release_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_release_release_example
    ADD CONSTRAINT l_release_release_example_pkey PRIMARY KEY (id);


--
-- Name: l_release_release_group_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_release_release_group_example
    ADD CONSTRAINT l_release_release_group_example_pkey PRIMARY KEY (id);


--
-- Name: l_release_url_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_release_url_example
    ADD CONSTRAINT l_release_url_example_pkey PRIMARY KEY (id);


--
-- Name: l_release_work_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_release_work_example
    ADD CONSTRAINT l_release_work_example_pkey PRIMARY KEY (id);


--
-- Name: l_url_url_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_url_url_example
    ADD CONSTRAINT l_url_url_example_pkey PRIMARY KEY (id);


--
-- Name: l_url_work_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_url_work_example
    ADD CONSTRAINT l_url_work_example_pkey PRIMARY KEY (id);


--
-- Name: l_work_work_example_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_work_work_example
    ADD CONSTRAINT l_work_work_example_pkey PRIMARY KEY (id);


--
-- Name: link_type_documentation_pkey; Type: CONSTRAINT; Schema: documentation; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY link_type_documentation
    ADD CONSTRAINT link_type_documentation_pkey PRIMARY KEY (id);


SET search_path = musicbrainz, pg_catalog;

--
-- Name: annotation_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY annotation
    ADD CONSTRAINT annotation_pkey PRIMARY KEY (id);


--
-- Name: application_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY application
    ADD CONSTRAINT application_pkey PRIMARY KEY (id);


--
-- Name: area_alias_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY area_alias
    ADD CONSTRAINT area_alias_pkey PRIMARY KEY (id);


--
-- Name: area_alias_type_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY area_alias_type
    ADD CONSTRAINT area_alias_type_pkey PRIMARY KEY (id);


--
-- Name: area_annotation_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY area_annotation
    ADD CONSTRAINT area_annotation_pkey PRIMARY KEY (area, annotation);


--
-- Name: area_gid_redirect_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY area_gid_redirect
    ADD CONSTRAINT area_gid_redirect_pkey PRIMARY KEY (gid);


--
-- Name: area_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY area
    ADD CONSTRAINT area_pkey PRIMARY KEY (id);


--
-- Name: area_type_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY area_type
    ADD CONSTRAINT area_type_pkey PRIMARY KEY (id);


--
-- Name: artist_alias_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist_alias
    ADD CONSTRAINT artist_alias_pkey PRIMARY KEY (id);


--
-- Name: artist_alias_type_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist_alias_type
    ADD CONSTRAINT artist_alias_type_pkey PRIMARY KEY (id);


--
-- Name: artist_annotation_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist_annotation
    ADD CONSTRAINT artist_annotation_pkey PRIMARY KEY (artist, annotation);


--
-- Name: artist_credit_name_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist_credit_name
    ADD CONSTRAINT artist_credit_name_pkey PRIMARY KEY (artist_credit, "position");


--
-- Name: artist_credit_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist_credit
    ADD CONSTRAINT artist_credit_pkey PRIMARY KEY (id);


--
-- Name: artist_deletion_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist_deletion
    ADD CONSTRAINT artist_deletion_pkey PRIMARY KEY (gid);


--
-- Name: artist_gid_redirect_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist_gid_redirect
    ADD CONSTRAINT artist_gid_redirect_pkey PRIMARY KEY (gid);


--
-- Name: artist_ipi_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist_ipi
    ADD CONSTRAINT artist_ipi_pkey PRIMARY KEY (artist, ipi);


--
-- Name: artist_isni_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist_isni
    ADD CONSTRAINT artist_isni_pkey PRIMARY KEY (artist, isni);


--
-- Name: artist_meta_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist_meta
    ADD CONSTRAINT artist_meta_pkey PRIMARY KEY (id);


--
-- Name: artist_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist
    ADD CONSTRAINT artist_pkey PRIMARY KEY (id);


--
-- Name: artist_rating_raw_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist_rating_raw
    ADD CONSTRAINT artist_rating_raw_pkey PRIMARY KEY (artist, editor);


--
-- Name: artist_tag_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist_tag
    ADD CONSTRAINT artist_tag_pkey PRIMARY KEY (artist, tag);


--
-- Name: artist_tag_raw_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist_tag_raw
    ADD CONSTRAINT artist_tag_raw_pkey PRIMARY KEY (artist, editor, tag);


--
-- Name: artist_type_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY artist_type
    ADD CONSTRAINT artist_type_pkey PRIMARY KEY (id);


--
-- Name: autoeditor_election_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY autoeditor_election
    ADD CONSTRAINT autoeditor_election_pkey PRIMARY KEY (id);


--
-- Name: autoeditor_election_vote_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY autoeditor_election_vote
    ADD CONSTRAINT autoeditor_election_vote_pkey PRIMARY KEY (id);


--
-- Name: cdtoc_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY cdtoc
    ADD CONSTRAINT cdtoc_pkey PRIMARY KEY (id);


--
-- Name: cdtoc_raw_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY cdtoc_raw
    ADD CONSTRAINT cdtoc_raw_pkey PRIMARY KEY (id);


--
-- Name: country_area_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY country_area
    ADD CONSTRAINT country_area_pkey PRIMARY KEY (area);


--
-- Name: edit_area_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY edit_area
    ADD CONSTRAINT edit_area_pkey PRIMARY KEY (edit, area);


--
-- Name: edit_artist_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY edit_artist
    ADD CONSTRAINT edit_artist_pkey PRIMARY KEY (edit, artist);


--
-- Name: edit_label_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY edit_label
    ADD CONSTRAINT edit_label_pkey PRIMARY KEY (edit, label);


--
-- Name: edit_note_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY edit_note
    ADD CONSTRAINT edit_note_pkey PRIMARY KEY (id);


--
-- Name: edit_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY edit
    ADD CONSTRAINT edit_pkey PRIMARY KEY (id);


--
-- Name: edit_recording_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY edit_recording
    ADD CONSTRAINT edit_recording_pkey PRIMARY KEY (edit, recording);


--
-- Name: edit_release_group_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY edit_release_group
    ADD CONSTRAINT edit_release_group_pkey PRIMARY KEY (edit, release_group);


--
-- Name: edit_release_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY edit_release
    ADD CONSTRAINT edit_release_pkey PRIMARY KEY (edit, release);


--
-- Name: edit_url_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY edit_url
    ADD CONSTRAINT edit_url_pkey PRIMARY KEY (edit, url);


--
-- Name: edit_work_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY edit_work
    ADD CONSTRAINT edit_work_pkey PRIMARY KEY (edit, work);


--
-- Name: editor_collection_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY editor_collection
    ADD CONSTRAINT editor_collection_pkey PRIMARY KEY (id);


--
-- Name: editor_collection_release_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY editor_collection_release
    ADD CONSTRAINT editor_collection_release_pkey PRIMARY KEY (collection, release);


--
-- Name: editor_language_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY editor_language
    ADD CONSTRAINT editor_language_pkey PRIMARY KEY (editor, language);


--
-- Name: editor_oauth_token_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY editor_oauth_token
    ADD CONSTRAINT editor_oauth_token_pkey PRIMARY KEY (id);


--
-- Name: editor_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY editor
    ADD CONSTRAINT editor_pkey PRIMARY KEY (id);


--
-- Name: editor_preference_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY editor_preference
    ADD CONSTRAINT editor_preference_pkey PRIMARY KEY (id);


--
-- Name: editor_subscribe_artist_deleted_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY editor_subscribe_artist_deleted
    ADD CONSTRAINT editor_subscribe_artist_deleted_pkey PRIMARY KEY (editor, gid);


--
-- Name: editor_subscribe_artist_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY editor_subscribe_artist
    ADD CONSTRAINT editor_subscribe_artist_pkey PRIMARY KEY (id);


--
-- Name: editor_subscribe_collection_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY editor_subscribe_collection
    ADD CONSTRAINT editor_subscribe_collection_pkey PRIMARY KEY (id);


--
-- Name: editor_subscribe_editor_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY editor_subscribe_editor
    ADD CONSTRAINT editor_subscribe_editor_pkey PRIMARY KEY (id);


--
-- Name: editor_subscribe_label_deleted_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY editor_subscribe_label_deleted
    ADD CONSTRAINT editor_subscribe_label_deleted_pkey PRIMARY KEY (editor, gid);


--
-- Name: editor_subscribe_label_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY editor_subscribe_label
    ADD CONSTRAINT editor_subscribe_label_pkey PRIMARY KEY (id);


--
-- Name: editor_watch_artist_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY editor_watch_artist
    ADD CONSTRAINT editor_watch_artist_pkey PRIMARY KEY (artist, editor);


--
-- Name: editor_watch_preferences_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY editor_watch_preferences
    ADD CONSTRAINT editor_watch_preferences_pkey PRIMARY KEY (editor);


--
-- Name: editor_watch_release_group_type_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY editor_watch_release_group_type
    ADD CONSTRAINT editor_watch_release_group_type_pkey PRIMARY KEY (editor, release_group_type);


--
-- Name: editor_watch_release_status_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY editor_watch_release_status
    ADD CONSTRAINT editor_watch_release_status_pkey PRIMARY KEY (editor, release_status);


--
-- Name: gender_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY gender
    ADD CONSTRAINT gender_pkey PRIMARY KEY (id);


--
-- Name: iso_3166_1_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY iso_3166_1
    ADD CONSTRAINT iso_3166_1_pkey PRIMARY KEY (code);


--
-- Name: iso_3166_2_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY iso_3166_2
    ADD CONSTRAINT iso_3166_2_pkey PRIMARY KEY (code);


--
-- Name: iso_3166_3_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY iso_3166_3
    ADD CONSTRAINT iso_3166_3_pkey PRIMARY KEY (code);


--
-- Name: isrc_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY isrc
    ADD CONSTRAINT isrc_pkey PRIMARY KEY (id);


--
-- Name: iswc_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY iswc
    ADD CONSTRAINT iswc_pkey PRIMARY KEY (id);


--
-- Name: l_area_area_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_area_area
    ADD CONSTRAINT l_area_area_pkey PRIMARY KEY (id);


--
-- Name: l_area_artist_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_area_artist
    ADD CONSTRAINT l_area_artist_pkey PRIMARY KEY (id);


--
-- Name: l_area_label_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_area_label
    ADD CONSTRAINT l_area_label_pkey PRIMARY KEY (id);


--
-- Name: l_area_recording_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_area_recording
    ADD CONSTRAINT l_area_recording_pkey PRIMARY KEY (id);


--
-- Name: l_area_release_group_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_area_release_group
    ADD CONSTRAINT l_area_release_group_pkey PRIMARY KEY (id);


--
-- Name: l_area_release_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_area_release
    ADD CONSTRAINT l_area_release_pkey PRIMARY KEY (id);


--
-- Name: l_area_url_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_area_url
    ADD CONSTRAINT l_area_url_pkey PRIMARY KEY (id);


--
-- Name: l_area_work_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_area_work
    ADD CONSTRAINT l_area_work_pkey PRIMARY KEY (id);


--
-- Name: l_artist_artist_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_artist_artist
    ADD CONSTRAINT l_artist_artist_pkey PRIMARY KEY (id);


--
-- Name: l_artist_label_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_artist_label
    ADD CONSTRAINT l_artist_label_pkey PRIMARY KEY (id);


--
-- Name: l_artist_recording_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_artist_recording
    ADD CONSTRAINT l_artist_recording_pkey PRIMARY KEY (id);


--
-- Name: l_artist_release_group_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_artist_release_group
    ADD CONSTRAINT l_artist_release_group_pkey PRIMARY KEY (id);


--
-- Name: l_artist_release_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_artist_release
    ADD CONSTRAINT l_artist_release_pkey PRIMARY KEY (id);


--
-- Name: l_artist_url_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_artist_url
    ADD CONSTRAINT l_artist_url_pkey PRIMARY KEY (id);


--
-- Name: l_artist_work_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_artist_work
    ADD CONSTRAINT l_artist_work_pkey PRIMARY KEY (id);


--
-- Name: l_label_label_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_label_label
    ADD CONSTRAINT l_label_label_pkey PRIMARY KEY (id);


--
-- Name: l_label_recording_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_label_recording
    ADD CONSTRAINT l_label_recording_pkey PRIMARY KEY (id);


--
-- Name: l_label_release_group_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_label_release_group
    ADD CONSTRAINT l_label_release_group_pkey PRIMARY KEY (id);


--
-- Name: l_label_release_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_label_release
    ADD CONSTRAINT l_label_release_pkey PRIMARY KEY (id);


--
-- Name: l_label_url_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_label_url
    ADD CONSTRAINT l_label_url_pkey PRIMARY KEY (id);


--
-- Name: l_label_work_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_label_work
    ADD CONSTRAINT l_label_work_pkey PRIMARY KEY (id);


--
-- Name: l_recording_recording_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_recording_recording
    ADD CONSTRAINT l_recording_recording_pkey PRIMARY KEY (id);


--
-- Name: l_recording_release_group_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_recording_release_group
    ADD CONSTRAINT l_recording_release_group_pkey PRIMARY KEY (id);


--
-- Name: l_recording_release_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_recording_release
    ADD CONSTRAINT l_recording_release_pkey PRIMARY KEY (id);


--
-- Name: l_recording_url_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_recording_url
    ADD CONSTRAINT l_recording_url_pkey PRIMARY KEY (id);


--
-- Name: l_recording_work_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_recording_work
    ADD CONSTRAINT l_recording_work_pkey PRIMARY KEY (id);


--
-- Name: l_release_group_release_group_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_release_group_release_group
    ADD CONSTRAINT l_release_group_release_group_pkey PRIMARY KEY (id);


--
-- Name: l_release_group_url_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_release_group_url
    ADD CONSTRAINT l_release_group_url_pkey PRIMARY KEY (id);


--
-- Name: l_release_group_work_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_release_group_work
    ADD CONSTRAINT l_release_group_work_pkey PRIMARY KEY (id);


--
-- Name: l_release_release_group_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_release_release_group
    ADD CONSTRAINT l_release_release_group_pkey PRIMARY KEY (id);


--
-- Name: l_release_release_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_release_release
    ADD CONSTRAINT l_release_release_pkey PRIMARY KEY (id);


--
-- Name: l_release_url_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_release_url
    ADD CONSTRAINT l_release_url_pkey PRIMARY KEY (id);


--
-- Name: l_release_work_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_release_work
    ADD CONSTRAINT l_release_work_pkey PRIMARY KEY (id);


--
-- Name: l_url_url_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_url_url
    ADD CONSTRAINT l_url_url_pkey PRIMARY KEY (id);


--
-- Name: l_url_work_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_url_work
    ADD CONSTRAINT l_url_work_pkey PRIMARY KEY (id);


--
-- Name: l_work_work_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY l_work_work
    ADD CONSTRAINT l_work_work_pkey PRIMARY KEY (id);


--
-- Name: label_alias_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY label_alias
    ADD CONSTRAINT label_alias_pkey PRIMARY KEY (id);


--
-- Name: label_alias_type_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY label_alias_type
    ADD CONSTRAINT label_alias_type_pkey PRIMARY KEY (id);


--
-- Name: label_annotation_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY label_annotation
    ADD CONSTRAINT label_annotation_pkey PRIMARY KEY (label, annotation);


--
-- Name: label_deletion_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY label_deletion
    ADD CONSTRAINT label_deletion_pkey PRIMARY KEY (gid);


--
-- Name: label_gid_redirect_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY label_gid_redirect
    ADD CONSTRAINT label_gid_redirect_pkey PRIMARY KEY (gid);


--
-- Name: label_ipi_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY label_ipi
    ADD CONSTRAINT label_ipi_pkey PRIMARY KEY (label, ipi);


--
-- Name: label_isni_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY label_isni
    ADD CONSTRAINT label_isni_pkey PRIMARY KEY (label, isni);


--
-- Name: label_meta_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY label_meta
    ADD CONSTRAINT label_meta_pkey PRIMARY KEY (id);


--
-- Name: label_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY label
    ADD CONSTRAINT label_pkey PRIMARY KEY (id);


--
-- Name: label_rating_raw_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY label_rating_raw
    ADD CONSTRAINT label_rating_raw_pkey PRIMARY KEY (label, editor);


--
-- Name: label_tag_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY label_tag
    ADD CONSTRAINT label_tag_pkey PRIMARY KEY (label, tag);


--
-- Name: label_tag_raw_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY label_tag_raw
    ADD CONSTRAINT label_tag_raw_pkey PRIMARY KEY (label, editor, tag);


--
-- Name: label_type_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY label_type
    ADD CONSTRAINT label_type_pkey PRIMARY KEY (id);


--
-- Name: language_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY language
    ADD CONSTRAINT language_pkey PRIMARY KEY (id);


--
-- Name: link_attribute_credit_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY link_attribute_credit
    ADD CONSTRAINT link_attribute_credit_pkey PRIMARY KEY (link, attribute_type);


--
-- Name: link_attribute_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY link_attribute
    ADD CONSTRAINT link_attribute_pkey PRIMARY KEY (link, attribute_type);


--
-- Name: link_attribute_type_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY link_attribute_type
    ADD CONSTRAINT link_attribute_type_pkey PRIMARY KEY (id);


--
-- Name: link_creditable_attribute_type_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY link_creditable_attribute_type
    ADD CONSTRAINT link_creditable_attribute_type_pkey PRIMARY KEY (attribute_type);


--
-- Name: link_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY link
    ADD CONSTRAINT link_pkey PRIMARY KEY (id);


--
-- Name: link_type_attribute_type_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY link_type_attribute_type
    ADD CONSTRAINT link_type_attribute_type_pkey PRIMARY KEY (link_type, attribute_type);


--
-- Name: link_type_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY link_type
    ADD CONSTRAINT link_type_pkey PRIMARY KEY (id);


--
-- Name: medium_cdtoc_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY medium_cdtoc
    ADD CONSTRAINT medium_cdtoc_pkey PRIMARY KEY (id);


--
-- Name: medium_format_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY medium_format
    ADD CONSTRAINT medium_format_pkey PRIMARY KEY (id);


--
-- Name: medium_index_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY medium_index
    ADD CONSTRAINT medium_index_pkey PRIMARY KEY (medium);


--
-- Name: medium_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY medium
    ADD CONSTRAINT medium_pkey PRIMARY KEY (id);


--
-- Name: recording_annotation_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY recording_annotation
    ADD CONSTRAINT recording_annotation_pkey PRIMARY KEY (recording, annotation);


--
-- Name: recording_gid_redirect_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY recording_gid_redirect
    ADD CONSTRAINT recording_gid_redirect_pkey PRIMARY KEY (gid);


--
-- Name: recording_meta_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY recording_meta
    ADD CONSTRAINT recording_meta_pkey PRIMARY KEY (id);


--
-- Name: recording_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY recording
    ADD CONSTRAINT recording_pkey PRIMARY KEY (id);


--
-- Name: recording_rating_raw_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY recording_rating_raw
    ADD CONSTRAINT recording_rating_raw_pkey PRIMARY KEY (recording, editor);


--
-- Name: recording_tag_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY recording_tag
    ADD CONSTRAINT recording_tag_pkey PRIMARY KEY (recording, tag);


--
-- Name: recording_tag_raw_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY recording_tag_raw
    ADD CONSTRAINT recording_tag_raw_pkey PRIMARY KEY (recording, editor, tag);


--
-- Name: release_annotation_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_annotation
    ADD CONSTRAINT release_annotation_pkey PRIMARY KEY (release, annotation);


--
-- Name: release_country_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_country
    ADD CONSTRAINT release_country_pkey PRIMARY KEY (release, country);


--
-- Name: release_coverart_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_coverart
    ADD CONSTRAINT release_coverart_pkey PRIMARY KEY (id);


--
-- Name: release_gid_redirect_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_gid_redirect
    ADD CONSTRAINT release_gid_redirect_pkey PRIMARY KEY (gid);


--
-- Name: release_group_annotation_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_group_annotation
    ADD CONSTRAINT release_group_annotation_pkey PRIMARY KEY (release_group, annotation);


--
-- Name: release_group_gid_redirect_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_group_gid_redirect
    ADD CONSTRAINT release_group_gid_redirect_pkey PRIMARY KEY (gid);


--
-- Name: release_group_meta_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_group_meta
    ADD CONSTRAINT release_group_meta_pkey PRIMARY KEY (id);


--
-- Name: release_group_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_group
    ADD CONSTRAINT release_group_pkey PRIMARY KEY (id);


--
-- Name: release_group_primary_type_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_group_primary_type
    ADD CONSTRAINT release_group_primary_type_pkey PRIMARY KEY (id);


--
-- Name: release_group_rating_raw_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_group_rating_raw
    ADD CONSTRAINT release_group_rating_raw_pkey PRIMARY KEY (release_group, editor);


--
-- Name: release_group_secondary_type_join_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_group_secondary_type_join
    ADD CONSTRAINT release_group_secondary_type_join_pkey PRIMARY KEY (release_group, secondary_type);


--
-- Name: release_group_secondary_type_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_group_secondary_type
    ADD CONSTRAINT release_group_secondary_type_pkey PRIMARY KEY (id);


--
-- Name: release_group_tag_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_group_tag
    ADD CONSTRAINT release_group_tag_pkey PRIMARY KEY (release_group, tag);


--
-- Name: release_group_tag_raw_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_group_tag_raw
    ADD CONSTRAINT release_group_tag_raw_pkey PRIMARY KEY (release_group, editor, tag);


--
-- Name: release_label_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_label
    ADD CONSTRAINT release_label_pkey PRIMARY KEY (id);


--
-- Name: release_meta_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_meta
    ADD CONSTRAINT release_meta_pkey PRIMARY KEY (id);


--
-- Name: release_packaging_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_packaging
    ADD CONSTRAINT release_packaging_pkey PRIMARY KEY (id);


--
-- Name: release_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release
    ADD CONSTRAINT release_pkey PRIMARY KEY (id);


--
-- Name: release_raw_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_raw
    ADD CONSTRAINT release_raw_pkey PRIMARY KEY (id);


--
-- Name: release_status_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_status
    ADD CONSTRAINT release_status_pkey PRIMARY KEY (id);


--
-- Name: release_tag_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_tag
    ADD CONSTRAINT release_tag_pkey PRIMARY KEY (release, tag);


--
-- Name: release_tag_raw_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_tag_raw
    ADD CONSTRAINT release_tag_raw_pkey PRIMARY KEY (release, editor, tag);


--
-- Name: release_unknown_country_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY release_unknown_country
    ADD CONSTRAINT release_unknown_country_pkey PRIMARY KEY (release);


--
-- Name: replication_control_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY replication_control
    ADD CONSTRAINT replication_control_pkey PRIMARY KEY (id);


--
-- Name: script_language_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY script_language
    ADD CONSTRAINT script_language_pkey PRIMARY KEY (id);


--
-- Name: script_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY script
    ADD CONSTRAINT script_pkey PRIMARY KEY (id);


--
-- Name: tag_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY tag
    ADD CONSTRAINT tag_pkey PRIMARY KEY (id);


--
-- Name: tag_relation_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY tag_relation
    ADD CONSTRAINT tag_relation_pkey PRIMARY KEY (tag1, tag2);


--
-- Name: track_gid_redirect_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY track_gid_redirect
    ADD CONSTRAINT track_gid_redirect_pkey PRIMARY KEY (gid);


--
-- Name: track_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY track
    ADD CONSTRAINT track_pkey PRIMARY KEY (id);


--
-- Name: track_raw_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY track_raw
    ADD CONSTRAINT track_raw_pkey PRIMARY KEY (id);


--
-- Name: url_gid_redirect_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY url_gid_redirect
    ADD CONSTRAINT url_gid_redirect_pkey PRIMARY KEY (gid);


--
-- Name: url_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY url
    ADD CONSTRAINT url_pkey PRIMARY KEY (id);


--
-- Name: vote_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY vote
    ADD CONSTRAINT vote_pkey PRIMARY KEY (id);


--
-- Name: work_alias_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY work_alias
    ADD CONSTRAINT work_alias_pkey PRIMARY KEY (id);


--
-- Name: work_alias_type_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY work_alias_type
    ADD CONSTRAINT work_alias_type_pkey PRIMARY KEY (id);


--
-- Name: work_annotation_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY work_annotation
    ADD CONSTRAINT work_annotation_pkey PRIMARY KEY (work, annotation);


--
-- Name: work_attribute_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY work_attribute
    ADD CONSTRAINT work_attribute_pkey PRIMARY KEY (id);


--
-- Name: work_attribute_type_allowed_value_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY work_attribute_type_allowed_value
    ADD CONSTRAINT work_attribute_type_allowed_value_pkey PRIMARY KEY (id);


--
-- Name: work_attribute_type_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY work_attribute_type
    ADD CONSTRAINT work_attribute_type_pkey PRIMARY KEY (id);


--
-- Name: work_gid_redirect_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY work_gid_redirect
    ADD CONSTRAINT work_gid_redirect_pkey PRIMARY KEY (gid);


--
-- Name: work_meta_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY work_meta
    ADD CONSTRAINT work_meta_pkey PRIMARY KEY (id);


--
-- Name: work_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY work
    ADD CONSTRAINT work_pkey PRIMARY KEY (id);


--
-- Name: work_rating_raw_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY work_rating_raw
    ADD CONSTRAINT work_rating_raw_pkey PRIMARY KEY (work, editor);


--
-- Name: work_tag_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY work_tag
    ADD CONSTRAINT work_tag_pkey PRIMARY KEY (work, tag);


--
-- Name: work_tag_raw_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY work_tag_raw
    ADD CONSTRAINT work_tag_raw_pkey PRIMARY KEY (work, editor, tag);


--
-- Name: work_type_pkey; Type: CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY work_type
    ADD CONSTRAINT work_type_pkey PRIMARY KEY (id);


SET search_path = report, pg_catalog;

--
-- Name: index_pkey; Type: CONSTRAINT; Schema: report; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY index
    ADD CONSTRAINT index_pkey PRIMARY KEY (report_name);


SET search_path = statistics, pg_catalog;

--
-- Name: log_statistic_pkey; Type: CONSTRAINT; Schema: statistics; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY log_statistic
    ADD CONSTRAINT log_statistic_pkey PRIMARY KEY (name, category, "timestamp");


--
-- Name: statistic_event_pkey; Type: CONSTRAINT; Schema: statistics; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY statistic_event
    ADD CONSTRAINT statistic_event_pkey PRIMARY KEY (date);


--
-- Name: statistic_pkey; Type: CONSTRAINT; Schema: statistics; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY statistic
    ADD CONSTRAINT statistic_pkey PRIMARY KEY (id);


SET search_path = wikidocs, pg_catalog;

--
-- Name: wikidocs_index_pkey; Type: CONSTRAINT; Schema: wikidocs; Owner: musicbrainz; Tablespace: 
--

ALTER TABLE ONLY wikidocs_index
    ADD CONSTRAINT wikidocs_index_pkey PRIMARY KEY (page_name);


SET search_path = cover_art_archive, pg_catalog;

--
-- Name: cover_art_idx_release; Type: INDEX; Schema: cover_art_archive; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX cover_art_idx_release ON cover_art USING btree (release);


SET search_path = musicbrainz, pg_catalog;

--
-- Name: application_idx_oauth_id; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX application_idx_oauth_id ON application USING btree (oauth_id);


--
-- Name: application_idx_owner; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX application_idx_owner ON application USING btree (owner);


--
-- Name: area_alias_idx_area; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX area_alias_idx_area ON area_alias USING btree (area);


--
-- Name: area_alias_idx_primary; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX area_alias_idx_primary ON area_alias USING btree (area, locale) WHERE ((primary_for_locale = true) AND (locale IS NOT NULL));


--
-- Name: area_idx_gid; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX area_idx_gid ON area USING btree (gid);


--
-- Name: area_idx_name; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX area_idx_name ON area USING btree (name);


--
-- Name: area_idx_name_txt; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX area_idx_name_txt ON area USING gin (to_tsvector('mb_simple'::regconfig, (name)::text));


--
-- Name: area_idx_page; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX area_idx_page ON area USING btree (page_index(name));


--
-- Name: area_idx_sort_name; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX area_idx_sort_name ON area USING btree (sort_name);


--
-- Name: artist_alias_idx_artist; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX artist_alias_idx_artist ON artist_alias USING btree (artist);


--
-- Name: artist_alias_idx_primary; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX artist_alias_idx_primary ON artist_alias USING btree (artist, locale) WHERE ((primary_for_locale = true) AND (locale IS NOT NULL));


--
-- Name: artist_credit_idx_musicbrainz_collate; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX artist_credit_idx_musicbrainz_collate ON artist_credit USING btree (musicbrainz_collate((name)::text));


--
-- Name: artist_credit_idx_txt; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX artist_credit_idx_txt ON artist_credit USING gin (to_tsvector('mb_simple'::regconfig, (name)::text));


--
-- Name: artist_credit_name_idx_artist; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX artist_credit_name_idx_artist ON artist_credit_name USING btree (artist);


--
-- Name: artist_credit_name_idx_musicbrainz_collate; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX artist_credit_name_idx_musicbrainz_collate ON artist_credit_name USING btree (musicbrainz_collate((name)::text));


--
-- Name: artist_credit_name_idx_txt; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX artist_credit_name_idx_txt ON artist_credit_name USING gin (to_tsvector('mb_simple'::regconfig, (name)::text));


--
-- Name: artist_idx_area; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX artist_idx_area ON artist USING btree (area);


--
-- Name: artist_idx_begin_area; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX artist_idx_begin_area ON artist USING btree (begin_area);


--
-- Name: artist_idx_end_area; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX artist_idx_end_area ON artist USING btree (end_area);


--
-- Name: artist_idx_gid; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX artist_idx_gid ON artist USING btree (gid);


--
-- Name: artist_idx_lower_name; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX artist_idx_lower_name ON artist USING btree (lower((name)::text));


--
-- Name: artist_idx_musicbrainz_collate; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX artist_idx_musicbrainz_collate ON artist USING btree (musicbrainz_collate((name)::text));


--
-- Name: artist_idx_name; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX artist_idx_name ON artist USING btree (name);


--
-- Name: artist_idx_null_comment; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX artist_idx_null_comment ON artist USING btree (name) WHERE (comment IS NULL);


--
-- Name: artist_idx_page; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX artist_idx_page ON artist USING btree (page_index(name));


--
-- Name: artist_idx_sort_name; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX artist_idx_sort_name ON artist USING btree (sort_name);


--
-- Name: artist_idx_txt; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX artist_idx_txt ON artist USING gin (to_tsvector('mb_simple'::regconfig, (name)::text));


--
-- Name: artist_idx_uniq_name_comment; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX artist_idx_uniq_name_comment ON artist USING btree (name, comment) WHERE (comment IS NOT NULL);


--
-- Name: artist_rating_raw_idx_artist; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX artist_rating_raw_idx_artist ON artist_rating_raw USING btree (artist);


--
-- Name: artist_rating_raw_idx_editor; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX artist_rating_raw_idx_editor ON artist_rating_raw USING btree (editor);


--
-- Name: artist_tag_idx_tag; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX artist_tag_idx_tag ON artist_tag USING btree (tag);


--
-- Name: artist_tag_raw_idx_editor; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX artist_tag_raw_idx_editor ON artist_tag_raw USING btree (editor);


--
-- Name: artist_tag_raw_idx_tag; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX artist_tag_raw_idx_tag ON artist_tag_raw USING btree (tag);


--
-- Name: cdtoc_idx_discid; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX cdtoc_idx_discid ON cdtoc USING btree (discid);


--
-- Name: cdtoc_idx_freedb_id; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX cdtoc_idx_freedb_id ON cdtoc USING btree (freedb_id);


--
-- Name: cdtoc_raw_discid; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX cdtoc_raw_discid ON cdtoc_raw USING btree (discid);


--
-- Name: cdtoc_raw_toc; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX cdtoc_raw_toc ON cdtoc_raw USING btree (track_count, leadout_offset, track_offset);


--
-- Name: cdtoc_raw_track_offset; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX cdtoc_raw_track_offset ON cdtoc_raw USING btree (track_offset);


--
-- Name: edit_area_idx; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX edit_area_idx ON edit_area USING btree (area);


--
-- Name: edit_artist_idx; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX edit_artist_idx ON edit_artist USING btree (artist);


--
-- Name: edit_artist_idx_status; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX edit_artist_idx_status ON edit_artist USING btree (status);


--
-- Name: edit_close_time_date; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX edit_close_time_date ON edit USING btree (date_trunc('day'::text, timezone('UTC'::text, close_time)));


--
-- Name: edit_expire_time_date; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX edit_expire_time_date ON edit USING btree (date_trunc('day'::text, timezone('UTC'::text, expire_time)));


--
-- Name: edit_idx_editor; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX edit_idx_editor ON edit USING btree (editor);


--
-- Name: edit_idx_editor_id_desc; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX edit_idx_editor_id_desc ON edit USING btree (editor, id DESC);


--
-- Name: edit_idx_open_edits_open_time; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX edit_idx_open_edits_open_time ON edit USING btree (open_time) WHERE (status = 1);


--
-- Name: edit_idx_open_time; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX edit_idx_open_time ON edit USING btree (open_time);


--
-- Name: edit_idx_status; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX edit_idx_status ON edit USING btree (status) WHERE (status <> 2);


--
-- Name: edit_idx_type; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX edit_idx_type ON edit USING btree (type);


--
-- Name: edit_idx_vote_time; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX edit_idx_vote_time ON vote USING btree (vote_time);


--
-- Name: edit_label_idx; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX edit_label_idx ON edit_label USING btree (label);


--
-- Name: edit_label_idx_status; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX edit_label_idx_status ON edit_label USING btree (status);


--
-- Name: edit_note_idx_edit; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX edit_note_idx_edit ON edit_note USING btree (edit);


--
-- Name: edit_open_time_date; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX edit_open_time_date ON edit USING btree (date_trunc('day'::text, timezone('UTC'::text, open_time)));


--
-- Name: edit_recording_idx; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX edit_recording_idx ON edit_recording USING btree (recording);


--
-- Name: edit_release_group_idx; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX edit_release_group_idx ON edit_release_group USING btree (release_group);


--
-- Name: edit_release_idx; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX edit_release_idx ON edit_release USING btree (release);


--
-- Name: edit_url_idx; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX edit_url_idx ON edit_url USING btree (url);


--
-- Name: edit_work_idx; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX edit_work_idx ON edit_work USING btree (work);


--
-- Name: editor_collection_idx_editor; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX editor_collection_idx_editor ON editor_collection USING btree (editor);


--
-- Name: editor_collection_idx_gid; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX editor_collection_idx_gid ON editor_collection USING btree (gid);


--
-- Name: editor_collection_idx_name; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX editor_collection_idx_name ON editor_collection USING btree (name);


--
-- Name: editor_idx_name; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX editor_idx_name ON editor USING btree (lower((name)::text));


--
-- Name: editor_language_idx_language; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX editor_language_idx_language ON editor_language USING btree (language);


--
-- Name: editor_oauth_token_idx_access_token; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX editor_oauth_token_idx_access_token ON editor_oauth_token USING btree (access_token);


--
-- Name: editor_oauth_token_idx_editor; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX editor_oauth_token_idx_editor ON editor_oauth_token USING btree (editor);


--
-- Name: editor_oauth_token_idx_refresh_token; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX editor_oauth_token_idx_refresh_token ON editor_oauth_token USING btree (refresh_token);


--
-- Name: editor_preference_idx_editor_name; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX editor_preference_idx_editor_name ON editor_preference USING btree (editor, name);


--
-- Name: editor_subscribe_artist_idx_artist; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX editor_subscribe_artist_idx_artist ON editor_subscribe_artist USING btree (artist);


--
-- Name: editor_subscribe_artist_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX editor_subscribe_artist_idx_uniq ON editor_subscribe_artist USING btree (editor, artist);


--
-- Name: editor_subscribe_collection_idx_collection; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX editor_subscribe_collection_idx_collection ON editor_subscribe_collection USING btree (collection);


--
-- Name: editor_subscribe_collection_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX editor_subscribe_collection_idx_uniq ON editor_subscribe_collection USING btree (editor, collection);


--
-- Name: editor_subscribe_editor_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX editor_subscribe_editor_idx_uniq ON editor_subscribe_editor USING btree (editor, subscribed_editor);


--
-- Name: editor_subscribe_label_idx_label; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX editor_subscribe_label_idx_label ON editor_subscribe_label USING btree (label);


--
-- Name: editor_subscribe_label_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX editor_subscribe_label_idx_uniq ON editor_subscribe_label USING btree (editor, label);


--
-- Name: iso_3166_1_idx_area; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX iso_3166_1_idx_area ON iso_3166_1 USING btree (area);


--
-- Name: iso_3166_2_idx_area; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX iso_3166_2_idx_area ON iso_3166_2 USING btree (area);


--
-- Name: iso_3166_3_idx_area; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX iso_3166_3_idx_area ON iso_3166_3 USING btree (area);


--
-- Name: isrc_idx_isrc; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX isrc_idx_isrc ON isrc USING btree (isrc);


--
-- Name: isrc_idx_isrc_recording; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX isrc_idx_isrc_recording ON isrc USING btree (isrc, recording);


--
-- Name: isrc_idx_recording; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX isrc_idx_recording ON isrc USING btree (recording);


--
-- Name: iswc_idx_iswc; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX iswc_idx_iswc ON iswc USING btree (iswc, work);


--
-- Name: iswc_idx_work; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX iswc_idx_work ON iswc USING btree (work);


--
-- Name: l_area_area_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_area_area_idx_entity1 ON l_area_area USING btree (entity1);


--
-- Name: l_area_area_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_area_area_idx_uniq ON l_area_area USING btree (entity0, entity1, link);


--
-- Name: l_area_artist_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_area_artist_idx_entity1 ON l_area_artist USING btree (entity1);


--
-- Name: l_area_artist_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_area_artist_idx_uniq ON l_area_artist USING btree (entity0, entity1, link);


--
-- Name: l_area_label_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_area_label_idx_entity1 ON l_area_label USING btree (entity1);


--
-- Name: l_area_label_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_area_label_idx_uniq ON l_area_label USING btree (entity0, entity1, link);


--
-- Name: l_area_recording_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_area_recording_idx_entity1 ON l_area_recording USING btree (entity1);


--
-- Name: l_area_recording_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_area_recording_idx_uniq ON l_area_recording USING btree (entity0, entity1, link);


--
-- Name: l_area_release_group_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_area_release_group_idx_entity1 ON l_area_release_group USING btree (entity1);


--
-- Name: l_area_release_group_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_area_release_group_idx_uniq ON l_area_release_group USING btree (entity0, entity1, link);


--
-- Name: l_area_release_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_area_release_idx_entity1 ON l_area_release USING btree (entity1);


--
-- Name: l_area_release_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_area_release_idx_uniq ON l_area_release USING btree (entity0, entity1, link);


--
-- Name: l_area_url_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_area_url_idx_entity1 ON l_area_url USING btree (entity1);


--
-- Name: l_area_url_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_area_url_idx_uniq ON l_area_url USING btree (entity0, entity1, link);


--
-- Name: l_area_work_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_area_work_idx_entity1 ON l_area_work USING btree (entity1);


--
-- Name: l_area_work_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_area_work_idx_uniq ON l_area_work USING btree (entity0, entity1, link);


--
-- Name: l_artist_artist_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_artist_artist_idx_entity1 ON l_artist_artist USING btree (entity1);


--
-- Name: l_artist_artist_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_artist_artist_idx_uniq ON l_artist_artist USING btree (entity0, entity1, link);


--
-- Name: l_artist_label_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_artist_label_idx_entity1 ON l_artist_label USING btree (entity1);


--
-- Name: l_artist_label_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_artist_label_idx_uniq ON l_artist_label USING btree (entity0, entity1, link);


--
-- Name: l_artist_recording_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_artist_recording_idx_entity1 ON l_artist_recording USING btree (entity1);


--
-- Name: l_artist_recording_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_artist_recording_idx_uniq ON l_artist_recording USING btree (entity0, entity1, link);


--
-- Name: l_artist_release_group_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_artist_release_group_idx_entity1 ON l_artist_release_group USING btree (entity1);


--
-- Name: l_artist_release_group_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_artist_release_group_idx_uniq ON l_artist_release_group USING btree (entity0, entity1, link);


--
-- Name: l_artist_release_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_artist_release_idx_entity1 ON l_artist_release USING btree (entity1);


--
-- Name: l_artist_release_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_artist_release_idx_uniq ON l_artist_release USING btree (entity0, entity1, link);


--
-- Name: l_artist_url_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_artist_url_idx_entity1 ON l_artist_url USING btree (entity1);


--
-- Name: l_artist_url_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_artist_url_idx_uniq ON l_artist_url USING btree (entity0, entity1, link);


--
-- Name: l_artist_work_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_artist_work_idx_entity1 ON l_artist_work USING btree (entity1);


--
-- Name: l_artist_work_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_artist_work_idx_uniq ON l_artist_work USING btree (entity0, entity1, link);


--
-- Name: l_label_label_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_label_label_idx_entity1 ON l_label_label USING btree (entity1);


--
-- Name: l_label_label_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_label_label_idx_uniq ON l_label_label USING btree (entity0, entity1, link);


--
-- Name: l_label_recording_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_label_recording_idx_entity1 ON l_label_recording USING btree (entity1);


--
-- Name: l_label_recording_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_label_recording_idx_uniq ON l_label_recording USING btree (entity0, entity1, link);


--
-- Name: l_label_release_group_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_label_release_group_idx_entity1 ON l_label_release_group USING btree (entity1);


--
-- Name: l_label_release_group_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_label_release_group_idx_uniq ON l_label_release_group USING btree (entity0, entity1, link);


--
-- Name: l_label_release_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_label_release_idx_entity1 ON l_label_release USING btree (entity1);


--
-- Name: l_label_release_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_label_release_idx_uniq ON l_label_release USING btree (entity0, entity1, link);


--
-- Name: l_label_url_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_label_url_idx_entity1 ON l_label_url USING btree (entity1);


--
-- Name: l_label_url_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_label_url_idx_uniq ON l_label_url USING btree (entity0, entity1, link);


--
-- Name: l_label_work_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_label_work_idx_entity1 ON l_label_work USING btree (entity1);


--
-- Name: l_label_work_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_label_work_idx_uniq ON l_label_work USING btree (entity0, entity1, link);


--
-- Name: l_recording_recording_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_recording_recording_idx_entity1 ON l_recording_recording USING btree (entity1);


--
-- Name: l_recording_recording_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_recording_recording_idx_uniq ON l_recording_recording USING btree (entity0, entity1, link);


--
-- Name: l_recording_release_group_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_recording_release_group_idx_entity1 ON l_recording_release_group USING btree (entity1);


--
-- Name: l_recording_release_group_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_recording_release_group_idx_uniq ON l_recording_release_group USING btree (entity0, entity1, link);


--
-- Name: l_recording_release_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_recording_release_idx_entity1 ON l_recording_release USING btree (entity1);


--
-- Name: l_recording_release_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_recording_release_idx_uniq ON l_recording_release USING btree (entity0, entity1, link);


--
-- Name: l_recording_url_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_recording_url_idx_entity1 ON l_recording_url USING btree (entity1);


--
-- Name: l_recording_url_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_recording_url_idx_uniq ON l_recording_url USING btree (entity0, entity1, link);


--
-- Name: l_recording_work_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_recording_work_idx_entity1 ON l_recording_work USING btree (entity1);


--
-- Name: l_recording_work_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_recording_work_idx_uniq ON l_recording_work USING btree (entity0, entity1, link);


--
-- Name: l_release_group_release_group_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_release_group_release_group_idx_entity1 ON l_release_group_release_group USING btree (entity1);


--
-- Name: l_release_group_release_group_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_release_group_release_group_idx_uniq ON l_release_group_release_group USING btree (entity0, entity1, link);


--
-- Name: l_release_group_url_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_release_group_url_idx_entity1 ON l_release_group_url USING btree (entity1);


--
-- Name: l_release_group_url_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_release_group_url_idx_uniq ON l_release_group_url USING btree (entity0, entity1, link);


--
-- Name: l_release_group_work_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_release_group_work_idx_entity1 ON l_release_group_work USING btree (entity1);


--
-- Name: l_release_group_work_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_release_group_work_idx_uniq ON l_release_group_work USING btree (entity0, entity1, link);


--
-- Name: l_release_release_group_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_release_release_group_idx_entity1 ON l_release_release_group USING btree (entity1);


--
-- Name: l_release_release_group_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_release_release_group_idx_uniq ON l_release_release_group USING btree (entity0, entity1, link);


--
-- Name: l_release_release_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_release_release_idx_entity1 ON l_release_release USING btree (entity1);


--
-- Name: l_release_release_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_release_release_idx_uniq ON l_release_release USING btree (entity0, entity1, link);


--
-- Name: l_release_url_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_release_url_idx_entity1 ON l_release_url USING btree (entity1);


--
-- Name: l_release_url_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_release_url_idx_uniq ON l_release_url USING btree (entity0, entity1, link);


--
-- Name: l_release_work_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_release_work_idx_entity1 ON l_release_work USING btree (entity1);


--
-- Name: l_release_work_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_release_work_idx_uniq ON l_release_work USING btree (entity0, entity1, link);


--
-- Name: l_url_url_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_url_url_idx_entity1 ON l_url_url USING btree (entity1);


--
-- Name: l_url_url_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_url_url_idx_uniq ON l_url_url USING btree (entity0, entity1, link);


--
-- Name: l_url_work_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_url_work_idx_entity1 ON l_url_work USING btree (entity1);


--
-- Name: l_url_work_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_url_work_idx_uniq ON l_url_work USING btree (entity0, entity1, link);


--
-- Name: l_work_work_idx_entity1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX l_work_work_idx_entity1 ON l_work_work USING btree (entity1);


--
-- Name: l_work_work_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX l_work_work_idx_uniq ON l_work_work USING btree (entity0, entity1, link);


--
-- Name: label_alias_idx_label; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX label_alias_idx_label ON label_alias USING btree (label);


--
-- Name: label_alias_idx_primary; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX label_alias_idx_primary ON label_alias USING btree (label, locale) WHERE ((primary_for_locale = true) AND (locale IS NOT NULL));


--
-- Name: label_idx_area; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX label_idx_area ON label USING btree (area);


--
-- Name: label_idx_gid; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX label_idx_gid ON label USING btree (gid);


--
-- Name: label_idx_lower_name; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX label_idx_lower_name ON label USING btree (lower((name)::text));


--
-- Name: label_idx_musicbrainz_collate; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX label_idx_musicbrainz_collate ON label USING btree (musicbrainz_collate((name)::text));


--
-- Name: label_idx_name; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX label_idx_name ON label USING btree (name);


--
-- Name: label_idx_null_comment; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX label_idx_null_comment ON label USING btree (name) WHERE (comment IS NULL);


--
-- Name: label_idx_page; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX label_idx_page ON label USING btree (page_index(name));


--
-- Name: label_idx_sort_name; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX label_idx_sort_name ON label USING btree (sort_name);


--
-- Name: label_idx_txt; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX label_idx_txt ON label USING gin (to_tsvector('mb_simple'::regconfig, (name)::text));


--
-- Name: label_idx_uniq_name_comment; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX label_idx_uniq_name_comment ON label USING btree (name, comment) WHERE (comment IS NOT NULL);


--
-- Name: label_rating_raw_idx_editor; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX label_rating_raw_idx_editor ON label_rating_raw USING btree (editor);


--
-- Name: label_rating_raw_idx_label; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX label_rating_raw_idx_label ON label_rating_raw USING btree (label);


--
-- Name: label_tag_idx_tag; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX label_tag_idx_tag ON label_tag USING btree (tag);


--
-- Name: label_tag_raw_idx_editor; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX label_tag_raw_idx_editor ON label_tag_raw USING btree (editor);


--
-- Name: label_tag_raw_idx_tag; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX label_tag_raw_idx_tag ON label_tag_raw USING btree (tag);


--
-- Name: language_idx_iso_code_1; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX language_idx_iso_code_1 ON language USING btree (iso_code_1);


--
-- Name: language_idx_iso_code_2b; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX language_idx_iso_code_2b ON language USING btree (iso_code_2b);


--
-- Name: language_idx_iso_code_2t; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX language_idx_iso_code_2t ON language USING btree (iso_code_2t);


--
-- Name: language_idx_iso_code_3; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX language_idx_iso_code_3 ON language USING btree (iso_code_3);


--
-- Name: link_attribute_type_idx_gid; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX link_attribute_type_idx_gid ON link_attribute_type USING btree (gid);


--
-- Name: link_idx_type_attr; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX link_idx_type_attr ON link USING btree (link_type, attribute_count);


--
-- Name: link_type_idx_gid; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX link_type_idx_gid ON link_type USING btree (gid);


--
-- Name: medium_cdtoc_idx_cdtoc; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX medium_cdtoc_idx_cdtoc ON medium_cdtoc USING btree (cdtoc);


--
-- Name: medium_cdtoc_idx_medium; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX medium_cdtoc_idx_medium ON medium_cdtoc USING btree (medium);


--
-- Name: medium_cdtoc_idx_uniq; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX medium_cdtoc_idx_uniq ON medium_cdtoc USING btree (medium, cdtoc);


--
-- Name: medium_idx_release; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX medium_idx_release ON medium USING btree (release);


--
-- Name: medium_idx_track_count; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX medium_idx_track_count ON medium USING btree (track_count);


--
-- Name: medium_index_idx; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX medium_index_idx ON medium_index USING gist (toc);


--
-- Name: recording_idx_artist_credit; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX recording_idx_artist_credit ON recording USING btree (artist_credit);


--
-- Name: recording_idx_gid; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX recording_idx_gid ON recording USING btree (gid);


--
-- Name: recording_idx_musicbrainz_collate; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX recording_idx_musicbrainz_collate ON recording USING btree (musicbrainz_collate((name)::text));


--
-- Name: recording_idx_name; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX recording_idx_name ON recording USING btree (name);


--
-- Name: recording_idx_txt; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX recording_idx_txt ON recording USING gin (to_tsvector('mb_simple'::regconfig, (name)::text));


--
-- Name: recording_rating_raw_idx_editor; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX recording_rating_raw_idx_editor ON recording_rating_raw USING btree (editor);


--
-- Name: recording_tag_idx_tag; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX recording_tag_idx_tag ON recording_tag USING btree (tag);


--
-- Name: recording_tag_raw_idx_editor; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX recording_tag_raw_idx_editor ON recording_tag_raw USING btree (editor);


--
-- Name: recording_tag_raw_idx_tag; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX recording_tag_raw_idx_tag ON recording_tag_raw USING btree (tag);


--
-- Name: recording_tag_raw_idx_track; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX recording_tag_raw_idx_track ON recording_tag_raw USING btree (recording);


--
-- Name: release_country_idx_country; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX release_country_idx_country ON release_country USING btree (country);


--
-- Name: release_group_idx_artist_credit; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX release_group_idx_artist_credit ON release_group USING btree (artist_credit);


--
-- Name: release_group_idx_gid; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX release_group_idx_gid ON release_group USING btree (gid);


--
-- Name: release_group_idx_musicbrainz_collate; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX release_group_idx_musicbrainz_collate ON release_group USING btree (musicbrainz_collate((name)::text));


--
-- Name: release_group_idx_name; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX release_group_idx_name ON release_group USING btree (name);


--
-- Name: release_group_idx_page; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX release_group_idx_page ON release_group USING btree (page_index(name));


--
-- Name: release_group_idx_txt; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX release_group_idx_txt ON release_group USING gin (to_tsvector('mb_simple'::regconfig, (name)::text));


--
-- Name: release_group_rating_raw_idx_editor; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX release_group_rating_raw_idx_editor ON release_group_rating_raw USING btree (editor);


--
-- Name: release_group_rating_raw_idx_release_group; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX release_group_rating_raw_idx_release_group ON release_group_rating_raw USING btree (release_group);


--
-- Name: release_group_tag_idx_tag; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX release_group_tag_idx_tag ON release_group_tag USING btree (tag);


--
-- Name: release_group_tag_raw_idx_editor; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX release_group_tag_raw_idx_editor ON release_group_tag_raw USING btree (editor);


--
-- Name: release_group_tag_raw_idx_tag; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX release_group_tag_raw_idx_tag ON release_group_tag_raw USING btree (tag);


--
-- Name: release_idx_artist_credit; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX release_idx_artist_credit ON release USING btree (artist_credit);


--
-- Name: release_idx_gid; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX release_idx_gid ON release USING btree (gid);


--
-- Name: release_idx_musicbrainz_collate; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX release_idx_musicbrainz_collate ON release USING btree (musicbrainz_collate((name)::text));


--
-- Name: release_idx_name; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX release_idx_name ON release USING btree (name);


--
-- Name: release_idx_page; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX release_idx_page ON release USING btree (page_index(name));


--
-- Name: release_idx_release_group; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX release_idx_release_group ON release USING btree (release_group);


--
-- Name: release_idx_txt; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX release_idx_txt ON release USING gin (to_tsvector('mb_simple'::regconfig, (name)::text));


--
-- Name: release_label_idx_label; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX release_label_idx_label ON release_label USING btree (label);


--
-- Name: release_label_idx_release; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX release_label_idx_release ON release_label USING btree (release);


--
-- Name: release_raw_idx_last_modified; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX release_raw_idx_last_modified ON release_raw USING btree (last_modified);


--
-- Name: release_raw_idx_lookup_count; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX release_raw_idx_lookup_count ON release_raw USING btree (lookup_count);


--
-- Name: release_raw_idx_modify_count; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX release_raw_idx_modify_count ON release_raw USING btree (modify_count);


--
-- Name: release_tag_idx_tag; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX release_tag_idx_tag ON release_tag USING btree (tag);


--
-- Name: release_tag_raw_idx_editor; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX release_tag_raw_idx_editor ON release_tag_raw USING btree (editor);


--
-- Name: release_tag_raw_idx_tag; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX release_tag_raw_idx_tag ON release_tag_raw USING btree (tag);


--
-- Name: script_idx_iso_code; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX script_idx_iso_code ON script USING btree (iso_code);


--
-- Name: tag_idx_name; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX tag_idx_name ON tag USING btree (name);


--
-- Name: tag_idx_name_txt; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX tag_idx_name_txt ON tag USING gin (to_tsvector('mb_simple'::regconfig, (name)::text));


--
-- Name: track_idx_artist_credit; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX track_idx_artist_credit ON track USING btree (artist_credit);


--
-- Name: track_idx_gid; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX track_idx_gid ON track USING btree (gid);


--
-- Name: track_idx_medium; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX track_idx_medium ON track USING btree (medium, "position");


--
-- Name: track_idx_musicbrainz_collate; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX track_idx_musicbrainz_collate ON track USING btree (musicbrainz_collate((name)::text));


--
-- Name: track_idx_name; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX track_idx_name ON track USING btree (name);


--
-- Name: track_idx_recording; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX track_idx_recording ON track USING btree (recording);


--
-- Name: track_idx_txt; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX track_idx_txt ON track USING gin (to_tsvector('mb_simple'::regconfig, (name)::text));


--
-- Name: track_raw_idx_release; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX track_raw_idx_release ON track_raw USING btree (release);


--
-- Name: url_idx_gid; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX url_idx_gid ON url USING btree (gid);


--
-- Name: url_idx_url; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX url_idx_url ON url USING btree (url);


--
-- Name: vote_idx_edit; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX vote_idx_edit ON vote USING btree (edit);


--
-- Name: vote_idx_editor; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX vote_idx_editor ON vote USING btree (editor);


--
-- Name: work_alias_idx_primary; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX work_alias_idx_primary ON work_alias USING btree (work, locale) WHERE ((primary_for_locale = true) AND (locale IS NOT NULL));


--
-- Name: work_alias_idx_work; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX work_alias_idx_work ON work_alias USING btree (work);


--
-- Name: work_attribute_idx_work; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX work_attribute_idx_work ON work_attribute USING btree (work);


--
-- Name: work_attribute_type_allowed_value_idx_name; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX work_attribute_type_allowed_value_idx_name ON work_attribute_type_allowed_value USING btree (work_attribute_type);


--
-- Name: work_idx_gid; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX work_idx_gid ON work USING btree (gid);


--
-- Name: work_idx_musicbrainz_collate; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX work_idx_musicbrainz_collate ON work USING btree (musicbrainz_collate((name)::text));


--
-- Name: work_idx_name; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX work_idx_name ON work USING btree (name);


--
-- Name: work_idx_page; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX work_idx_page ON work USING btree (page_index(name));


--
-- Name: work_idx_txt; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX work_idx_txt ON work USING gin (to_tsvector('mb_simple'::regconfig, (name)::text));


--
-- Name: work_tag_idx_tag; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX work_tag_idx_tag ON work_tag USING btree (tag);


--
-- Name: work_tag_raw_idx_tag; Type: INDEX; Schema: musicbrainz; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX work_tag_raw_idx_tag ON work_tag_raw USING btree (tag);


SET search_path = statistics, pg_catalog;

--
-- Name: statistic_name; Type: INDEX; Schema: statistics; Owner: musicbrainz; Tablespace: 
--

CREATE INDEX statistic_name ON statistic USING btree (name);


--
-- Name: statistic_name_date_collected; Type: INDEX; Schema: statistics; Owner: musicbrainz; Tablespace: 
--

CREATE UNIQUE INDEX statistic_name_date_collected ON statistic USING btree (name, date_collected);


SET search_path = cover_art_archive, pg_catalog;

--
-- Name: resquence_cover_art; Type: TRIGGER; Schema: cover_art_archive; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER resquence_cover_art AFTER INSERT OR DELETE OR UPDATE ON cover_art DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE resequence_cover_art_trigger();


--
-- Name: update_release_coverart; Type: TRIGGER; Schema: cover_art_archive; Owner: musicbrainz
--

CREATE TRIGGER update_release_coverart AFTER INSERT OR DELETE ON cover_art FOR EACH ROW EXECUTE PROCEDURE materialize_caa_presence();


SET search_path = musicbrainz, pg_catalog;

--
-- Name: a_del_recording; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER a_del_recording AFTER DELETE ON recording FOR EACH ROW EXECUTE PROCEDURE a_del_recording();


--
-- Name: a_del_release; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER a_del_release AFTER DELETE ON release FOR EACH ROW EXECUTE PROCEDURE a_del_release();


--
-- Name: a_del_release_event; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER a_del_release_event AFTER DELETE ON release_country FOR EACH ROW EXECUTE PROCEDURE a_del_release_event();


--
-- Name: a_del_release_event; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER a_del_release_event AFTER DELETE ON release_unknown_country FOR EACH ROW EXECUTE PROCEDURE a_del_release_event();


--
-- Name: a_del_release_group; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER a_del_release_group AFTER DELETE ON release_group FOR EACH ROW EXECUTE PROCEDURE a_del_release_group();


--
-- Name: a_del_track; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER a_del_track AFTER DELETE ON track FOR EACH ROW EXECUTE PROCEDURE a_del_track();


--
-- Name: a_ins_artist; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER a_ins_artist AFTER INSERT ON artist FOR EACH ROW EXECUTE PROCEDURE a_ins_artist();


--
-- Name: a_ins_edit_artist; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER a_ins_edit_artist BEFORE INSERT ON edit_artist FOR EACH ROW EXECUTE PROCEDURE b_ins_edit_materialize_status();


--
-- Name: a_ins_edit_artist; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER a_ins_edit_artist BEFORE INSERT ON edit_label FOR EACH ROW EXECUTE PROCEDURE b_ins_edit_materialize_status();


--
-- Name: a_ins_editor; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER a_ins_editor AFTER INSERT ON editor FOR EACH ROW EXECUTE PROCEDURE a_ins_editor();


--
-- Name: a_ins_label; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER a_ins_label AFTER INSERT ON label FOR EACH ROW EXECUTE PROCEDURE a_ins_label();


--
-- Name: a_ins_recording; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER a_ins_recording AFTER INSERT ON recording FOR EACH ROW EXECUTE PROCEDURE a_ins_recording();


--
-- Name: a_ins_release; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER a_ins_release AFTER INSERT ON release FOR EACH ROW EXECUTE PROCEDURE a_ins_release();


--
-- Name: a_ins_release_event; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER a_ins_release_event AFTER INSERT ON release_country FOR EACH ROW EXECUTE PROCEDURE a_ins_release_event();


--
-- Name: a_ins_release_event; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER a_ins_release_event AFTER INSERT ON release_unknown_country FOR EACH ROW EXECUTE PROCEDURE a_ins_release_event();


--
-- Name: a_ins_release_group; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER a_ins_release_group AFTER INSERT ON release_group FOR EACH ROW EXECUTE PROCEDURE a_ins_release_group();


--
-- Name: a_ins_track; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER a_ins_track AFTER INSERT ON track FOR EACH ROW EXECUTE PROCEDURE a_ins_track();


--
-- Name: a_ins_work; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER a_ins_work AFTER INSERT ON work FOR EACH ROW EXECUTE PROCEDURE a_ins_work();


--
-- Name: a_upd_edit; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER a_upd_edit AFTER UPDATE ON edit FOR EACH ROW EXECUTE PROCEDURE a_upd_edit();


--
-- Name: a_upd_recording; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER a_upd_recording AFTER UPDATE ON recording FOR EACH ROW EXECUTE PROCEDURE a_upd_recording();


--
-- Name: a_upd_release; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER a_upd_release AFTER UPDATE ON release FOR EACH ROW EXECUTE PROCEDURE a_upd_release();


--
-- Name: a_upd_release_event; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER a_upd_release_event AFTER UPDATE ON release_country FOR EACH ROW EXECUTE PROCEDURE a_upd_release_event();


--
-- Name: a_upd_release_event; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER a_upd_release_event AFTER UPDATE ON release_unknown_country FOR EACH ROW EXECUTE PROCEDURE a_upd_release_event();


--
-- Name: a_upd_release_group; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER a_upd_release_group AFTER UPDATE ON release_group FOR EACH ROW EXECUTE PROCEDURE a_upd_release_group();


--
-- Name: a_upd_track; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER a_upd_track AFTER UPDATE ON track FOR EACH ROW EXECUTE PROCEDURE a_upd_track();


--
-- Name: b_del_artist_special; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_del_artist_special BEFORE DELETE ON artist FOR EACH ROW WHEN ((old.id = ANY (ARRAY[1, 2]))) EXECUTE PROCEDURE deny_special_purpose_deletion();


--
-- Name: b_del_label_special; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_del_label_special BEFORE DELETE ON label FOR EACH ROW WHEN ((old.id = 1)) EXECUTE PROCEDURE deny_special_purpose_deletion();


--
-- Name: b_upd_area; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_area BEFORE UPDATE ON area FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_area_alias; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_area_alias BEFORE UPDATE ON area_alias FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_artist; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_artist BEFORE UPDATE ON artist FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_artist_alias; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_artist_alias BEFORE UPDATE ON artist_alias FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_artist_tag; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_artist_tag BEFORE UPDATE ON artist_tag FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_editor; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_editor BEFORE UPDATE ON editor FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_area_area; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_area_area BEFORE UPDATE ON l_area_area FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_area_artist; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_area_artist BEFORE UPDATE ON l_area_artist FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_area_label; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_area_label BEFORE UPDATE ON l_area_label FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_area_recording; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_area_recording BEFORE UPDATE ON l_area_recording FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_area_release; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_area_release BEFORE UPDATE ON l_area_release FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_area_release_group; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_area_release_group BEFORE UPDATE ON l_area_release_group FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_area_url; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_area_url BEFORE UPDATE ON l_area_url FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_area_work; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_area_work BEFORE UPDATE ON l_area_work FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_artist_artist; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_artist_artist BEFORE UPDATE ON l_artist_artist FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_artist_label; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_artist_label BEFORE UPDATE ON l_artist_label FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_artist_recording; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_artist_recording BEFORE UPDATE ON l_artist_recording FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_artist_release; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_artist_release BEFORE UPDATE ON l_artist_release FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_artist_release_group; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_artist_release_group BEFORE UPDATE ON l_artist_release_group FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_artist_url; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_artist_url BEFORE UPDATE ON l_artist_url FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_artist_work; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_artist_work BEFORE UPDATE ON l_artist_work FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_label_label; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_label_label BEFORE UPDATE ON l_label_label FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_label_recording; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_label_recording BEFORE UPDATE ON l_label_recording FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_label_release; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_label_release BEFORE UPDATE ON l_label_release FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_label_release_group; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_label_release_group BEFORE UPDATE ON l_label_release_group FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_label_url; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_label_url BEFORE UPDATE ON l_label_url FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_label_work; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_label_work BEFORE UPDATE ON l_label_work FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_recording_recording; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_recording_recording BEFORE UPDATE ON l_recording_recording FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_recording_release; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_recording_release BEFORE UPDATE ON l_recording_release FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_recording_release_group; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_recording_release_group BEFORE UPDATE ON l_recording_release_group FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_recording_url; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_recording_url BEFORE UPDATE ON l_recording_url FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_recording_work; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_recording_work BEFORE UPDATE ON l_recording_work FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_release_group_release_group; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_release_group_release_group BEFORE UPDATE ON l_release_group_release_group FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_release_group_url; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_release_group_url BEFORE UPDATE ON l_release_group_url FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_release_group_work; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_release_group_work BEFORE UPDATE ON l_release_group_work FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_release_release; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_release_release BEFORE UPDATE ON l_release_release FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_release_release_group; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_release_release_group BEFORE UPDATE ON l_release_release_group FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_release_url; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_release_url BEFORE UPDATE ON l_release_url FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_release_work; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_release_work BEFORE UPDATE ON l_release_work FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_url_url; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_url_url BEFORE UPDATE ON l_url_url FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_url_work; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_url_work BEFORE UPDATE ON l_url_work FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_l_work_work; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_l_work_work BEFORE UPDATE ON l_work_work FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_label; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_label BEFORE UPDATE ON label FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_label_alias; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_label_alias BEFORE UPDATE ON label_alias FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_label_tag; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_label_tag BEFORE UPDATE ON label_tag FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_link_attribute; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_link_attribute BEFORE INSERT OR UPDATE ON link_attribute FOR EACH ROW EXECUTE PROCEDURE prevent_invalid_attributes();


--
-- Name: b_upd_link_attribute_type; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_link_attribute_type BEFORE UPDATE ON link_attribute_type FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_link_type; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_link_type BEFORE UPDATE ON link_type FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_link_type_attribute_type; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_link_type_attribute_type BEFORE UPDATE ON link_type_attribute_type FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_medium; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_medium BEFORE UPDATE ON medium FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_medium_cdtoc; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_medium_cdtoc BEFORE UPDATE ON medium_cdtoc FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_recording; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_recording BEFORE UPDATE ON recording FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_recording_tag; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_recording_tag BEFORE UPDATE ON recording_tag FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_release; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_release BEFORE UPDATE ON release FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_release_group; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_release_group BEFORE UPDATE ON release_group FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_release_group_tag; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_release_group_tag BEFORE UPDATE ON release_group_tag FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_release_label; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_release_label BEFORE UPDATE ON release_label FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_tag_relation; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_tag_relation BEFORE UPDATE ON tag_relation FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_track; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_track BEFORE UPDATE ON track FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_url; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_url BEFORE UPDATE ON url FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_work; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_work BEFORE UPDATE ON work FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_work_alias; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_work_alias BEFORE UPDATE ON work_alias FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: b_upd_work_tag; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER b_upd_work_tag BEFORE UPDATE ON work_tag FOR EACH ROW EXECUTE PROCEDURE b_upd_last_updated_table();


--
-- Name: del_collection_sub_on_delete; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER del_collection_sub_on_delete BEFORE DELETE ON editor_collection FOR EACH ROW EXECUTE PROCEDURE del_collection_sub_on_delete();


--
-- Name: del_collection_sub_on_private; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER del_collection_sub_on_private BEFORE UPDATE ON editor_collection FOR EACH ROW EXECUTE PROCEDURE del_collection_sub_on_private();


--
-- Name: delete_unused_tag; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER delete_unused_tag AFTER INSERT ON tag DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE trg_delete_unused_tag();


--
-- Name: delete_unused_tag; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER delete_unused_tag AFTER DELETE ON artist_tag DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE trg_delete_unused_tag_ref();


--
-- Name: delete_unused_tag; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER delete_unused_tag AFTER DELETE ON label_tag DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE trg_delete_unused_tag_ref();


--
-- Name: delete_unused_tag; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER delete_unused_tag AFTER DELETE ON release_group_tag DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE trg_delete_unused_tag_ref();


--
-- Name: delete_unused_tag; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER delete_unused_tag AFTER DELETE ON work_tag DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE trg_delete_unused_tag_ref();


--
-- Name: end_area_implies_ended; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER end_area_implies_ended BEFORE INSERT OR UPDATE ON artist FOR EACH ROW EXECUTE PROCEDURE end_area_implies_ended();


--
-- Name: end_date_implies_ended; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER end_date_implies_ended BEFORE INSERT OR UPDATE ON area FOR EACH ROW EXECUTE PROCEDURE end_date_implies_ended();


--
-- Name: end_date_implies_ended; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER end_date_implies_ended BEFORE INSERT OR UPDATE ON link FOR EACH ROW EXECUTE PROCEDURE end_date_implies_ended();


--
-- Name: end_date_implies_ended; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER end_date_implies_ended BEFORE INSERT OR UPDATE ON area_alias FOR EACH ROW EXECUTE PROCEDURE end_date_implies_ended();


--
-- Name: end_date_implies_ended; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER end_date_implies_ended BEFORE INSERT OR UPDATE ON artist_alias FOR EACH ROW EXECUTE PROCEDURE end_date_implies_ended();


--
-- Name: end_date_implies_ended; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER end_date_implies_ended BEFORE INSERT OR UPDATE ON label_alias FOR EACH ROW EXECUTE PROCEDURE end_date_implies_ended();


--
-- Name: end_date_implies_ended; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER end_date_implies_ended BEFORE INSERT OR UPDATE ON work_alias FOR EACH ROW EXECUTE PROCEDURE end_date_implies_ended();


--
-- Name: end_date_implies_ended; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER end_date_implies_ended BEFORE INSERT OR UPDATE ON artist FOR EACH ROW EXECUTE PROCEDURE end_date_implies_ended();


--
-- Name: end_date_implies_ended; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER end_date_implies_ended BEFORE INSERT OR UPDATE ON label FOR EACH ROW EXECUTE PROCEDURE end_date_implies_ended();


--
-- Name: ensure_work_attribute_type_allows_text; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER ensure_work_attribute_type_allows_text BEFORE INSERT OR UPDATE ON work_attribute FOR EACH ROW EXECUTE PROCEDURE ensure_work_attribute_type_allows_text();


--
-- Name: inserting_edits_requires_confirmed_email_address; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER inserting_edits_requires_confirmed_email_address BEFORE INSERT ON edit FOR EACH ROW EXECUTE PROCEDURE inserting_edits_requires_confirmed_email_address();


--
-- Name: remove_orphaned_tracks; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER remove_orphaned_tracks AFTER DELETE OR UPDATE ON track DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE delete_orphaned_recordings();


--
-- Name: remove_unused_links; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER remove_unused_links AFTER DELETE OR UPDATE ON l_artist_artist DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_links();


--
-- Name: remove_unused_links; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER remove_unused_links AFTER DELETE OR UPDATE ON l_artist_label DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_links();


--
-- Name: remove_unused_links; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER remove_unused_links AFTER DELETE OR UPDATE ON l_artist_recording DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_links();


--
-- Name: remove_unused_links; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER remove_unused_links AFTER DELETE OR UPDATE ON l_artist_release DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_links();


--
-- Name: remove_unused_links; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER remove_unused_links AFTER DELETE OR UPDATE ON l_artist_release_group DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_links();


--
-- Name: remove_unused_links; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER remove_unused_links AFTER DELETE OR UPDATE ON l_artist_url DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_links();


--
-- Name: remove_unused_links; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER remove_unused_links AFTER DELETE OR UPDATE ON l_artist_work DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_links();


--
-- Name: remove_unused_links; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER remove_unused_links AFTER DELETE OR UPDATE ON l_label_label DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_links();


--
-- Name: remove_unused_links; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER remove_unused_links AFTER DELETE OR UPDATE ON l_label_recording DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_links();


--
-- Name: remove_unused_links; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER remove_unused_links AFTER DELETE OR UPDATE ON l_label_release DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_links();


--
-- Name: remove_unused_links; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER remove_unused_links AFTER DELETE OR UPDATE ON l_label_release_group DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_links();


--
-- Name: remove_unused_links; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER remove_unused_links AFTER DELETE OR UPDATE ON l_label_url DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_links();


--
-- Name: remove_unused_links; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER remove_unused_links AFTER DELETE OR UPDATE ON l_label_work DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_links();


--
-- Name: remove_unused_links; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER remove_unused_links AFTER DELETE OR UPDATE ON l_recording_recording DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_links();


--
-- Name: remove_unused_links; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER remove_unused_links AFTER DELETE OR UPDATE ON l_recording_release DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_links();


--
-- Name: remove_unused_links; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER remove_unused_links AFTER DELETE OR UPDATE ON l_recording_release_group DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_links();


--
-- Name: remove_unused_links; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER remove_unused_links AFTER DELETE OR UPDATE ON l_recording_url DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_links();


--
-- Name: remove_unused_links; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER remove_unused_links AFTER DELETE OR UPDATE ON l_recording_work DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_links();


--
-- Name: remove_unused_links; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER remove_unused_links AFTER DELETE OR UPDATE ON l_release_release DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_links();


--
-- Name: remove_unused_links; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER remove_unused_links AFTER DELETE OR UPDATE ON l_release_release_group DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_links();


--
-- Name: remove_unused_links; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER remove_unused_links AFTER DELETE OR UPDATE ON l_release_url DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_links();


--
-- Name: remove_unused_links; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER remove_unused_links AFTER DELETE OR UPDATE ON l_release_work DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_links();


--
-- Name: remove_unused_links; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER remove_unused_links AFTER DELETE OR UPDATE ON l_release_group_release_group DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_links();


--
-- Name: remove_unused_links; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER remove_unused_links AFTER DELETE OR UPDATE ON l_release_group_url DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_links();


--
-- Name: remove_unused_links; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER remove_unused_links AFTER DELETE OR UPDATE ON l_release_group_work DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_links();


--
-- Name: remove_unused_links; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER remove_unused_links AFTER DELETE OR UPDATE ON l_url_url DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_links();


--
-- Name: remove_unused_links; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER remove_unused_links AFTER DELETE OR UPDATE ON l_url_work DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_links();


--
-- Name: remove_unused_links; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER remove_unused_links AFTER DELETE OR UPDATE ON l_work_work DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_links();


--
-- Name: replace_old_sub_on_add; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER replace_old_sub_on_add BEFORE INSERT ON editor_subscribe_collection FOR EACH ROW EXECUTE PROCEDURE replace_old_sub_on_add();


--
-- Name: search_hint; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER search_hint BEFORE INSERT OR UPDATE ON area_alias FOR EACH ROW EXECUTE PROCEDURE simplify_search_hints('3');


--
-- Name: search_hint; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER search_hint BEFORE INSERT OR UPDATE ON artist_alias FOR EACH ROW EXECUTE PROCEDURE simplify_search_hints('3');


--
-- Name: search_hint; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER search_hint BEFORE INSERT OR UPDATE ON label_alias FOR EACH ROW EXECUTE PROCEDURE simplify_search_hints('2');


--
-- Name: search_hint; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER search_hint BEFORE INSERT OR UPDATE ON work_alias FOR EACH ROW EXECUTE PROCEDURE simplify_search_hints('2');


--
-- Name: unique_primary_for_locale; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER unique_primary_for_locale BEFORE INSERT OR UPDATE ON area_alias FOR EACH ROW EXECUTE PROCEDURE unique_primary_area_alias();


--
-- Name: unique_primary_for_locale; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER unique_primary_for_locale BEFORE INSERT OR UPDATE ON artist_alias FOR EACH ROW EXECUTE PROCEDURE unique_primary_artist_alias();


--
-- Name: unique_primary_for_locale; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER unique_primary_for_locale BEFORE INSERT OR UPDATE ON label_alias FOR EACH ROW EXECUTE PROCEDURE unique_primary_label_alias();


--
-- Name: unique_primary_for_locale; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE TRIGGER unique_primary_for_locale BEFORE INSERT OR UPDATE ON work_alias FOR EACH ROW EXECUTE PROCEDURE unique_primary_work_alias();


--
-- Name: url_gc_a_del_l_artist_url; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER url_gc_a_del_l_artist_url AFTER DELETE ON l_artist_url DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_url();


--
-- Name: url_gc_a_del_l_label_url; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER url_gc_a_del_l_label_url AFTER DELETE ON l_label_url DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_url();


--
-- Name: url_gc_a_del_l_recording_url; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER url_gc_a_del_l_recording_url AFTER DELETE ON l_recording_url DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_url();


--
-- Name: url_gc_a_del_l_release_group_url; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER url_gc_a_del_l_release_group_url AFTER DELETE ON l_release_group_url DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_url();


--
-- Name: url_gc_a_del_l_release_url; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER url_gc_a_del_l_release_url AFTER DELETE ON l_release_url DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_url();


--
-- Name: url_gc_a_del_l_url_url; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER url_gc_a_del_l_url_url AFTER DELETE ON l_url_url DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_url();


--
-- Name: url_gc_a_del_l_url_work; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER url_gc_a_del_l_url_work AFTER DELETE ON l_url_work DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_url();


--
-- Name: url_gc_a_upd_l_artist_url; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER url_gc_a_upd_l_artist_url AFTER UPDATE ON l_artist_url DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_url();


--
-- Name: url_gc_a_upd_l_label_url; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER url_gc_a_upd_l_label_url AFTER UPDATE ON l_label_url DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_url();


--
-- Name: url_gc_a_upd_l_recording_url; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER url_gc_a_upd_l_recording_url AFTER UPDATE ON l_recording_url DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_url();


--
-- Name: url_gc_a_upd_l_release_group_url; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER url_gc_a_upd_l_release_group_url AFTER UPDATE ON l_release_group_url DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_url();


--
-- Name: url_gc_a_upd_l_release_url; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER url_gc_a_upd_l_release_url AFTER UPDATE ON l_release_url DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_url();


--
-- Name: url_gc_a_upd_l_url_url; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER url_gc_a_upd_l_url_url AFTER UPDATE ON l_url_url DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_url();


--
-- Name: url_gc_a_upd_l_url_work; Type: TRIGGER; Schema: musicbrainz; Owner: musicbrainz
--

CREATE CONSTRAINT TRIGGER url_gc_a_upd_l_url_work AFTER UPDATE ON l_url_work DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE remove_unused_url();


SET search_path = cover_art_archive, pg_catalog;

--
-- Name: cover_art_fk_edit; Type: FK CONSTRAINT; Schema: cover_art_archive; Owner: musicbrainz
--

ALTER TABLE ONLY cover_art
    ADD CONSTRAINT cover_art_fk_edit FOREIGN KEY (edit) REFERENCES musicbrainz.edit(id);


--
-- Name: cover_art_fk_mime_type; Type: FK CONSTRAINT; Schema: cover_art_archive; Owner: musicbrainz
--

ALTER TABLE ONLY cover_art
    ADD CONSTRAINT cover_art_fk_mime_type FOREIGN KEY (mime_type) REFERENCES image_type(mime_type);


--
-- Name: cover_art_fk_release; Type: FK CONSTRAINT; Schema: cover_art_archive; Owner: musicbrainz
--

ALTER TABLE ONLY cover_art
    ADD CONSTRAINT cover_art_fk_release FOREIGN KEY (release) REFERENCES musicbrainz.release(id) ON DELETE CASCADE;


--
-- Name: cover_art_type_fk_id; Type: FK CONSTRAINT; Schema: cover_art_archive; Owner: musicbrainz
--

ALTER TABLE ONLY cover_art_type
    ADD CONSTRAINT cover_art_type_fk_id FOREIGN KEY (id) REFERENCES cover_art(id) ON DELETE CASCADE;


--
-- Name: cover_art_type_fk_type_id; Type: FK CONSTRAINT; Schema: cover_art_archive; Owner: musicbrainz
--

ALTER TABLE ONLY cover_art_type
    ADD CONSTRAINT cover_art_type_fk_type_id FOREIGN KEY (type_id) REFERENCES art_type(id);


--
-- Name: release_group_cover_art_fk_release; Type: FK CONSTRAINT; Schema: cover_art_archive; Owner: musicbrainz
--

ALTER TABLE ONLY release_group_cover_art
    ADD CONSTRAINT release_group_cover_art_fk_release FOREIGN KEY (release) REFERENCES musicbrainz.release(id);


--
-- Name: release_group_cover_art_fk_release_group; Type: FK CONSTRAINT; Schema: cover_art_archive; Owner: musicbrainz
--

ALTER TABLE ONLY release_group_cover_art
    ADD CONSTRAINT release_group_cover_art_fk_release_group FOREIGN KEY (release_group) REFERENCES musicbrainz.release_group(id);


SET search_path = musicbrainz, pg_catalog;

--
-- Name: annotation_fk_editor; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY annotation
    ADD CONSTRAINT annotation_fk_editor FOREIGN KEY (editor) REFERENCES editor(id);


--
-- Name: application_fk_owner; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY application
    ADD CONSTRAINT application_fk_owner FOREIGN KEY (owner) REFERENCES editor(id);


--
-- Name: area_alias_fk_area; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY area_alias
    ADD CONSTRAINT area_alias_fk_area FOREIGN KEY (area) REFERENCES area(id);


--
-- Name: area_alias_fk_type; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY area_alias
    ADD CONSTRAINT area_alias_fk_type FOREIGN KEY (type) REFERENCES area_alias_type(id);


--
-- Name: area_annotation_fk_annotation; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY area_annotation
    ADD CONSTRAINT area_annotation_fk_annotation FOREIGN KEY (annotation) REFERENCES annotation(id);


--
-- Name: area_annotation_fk_area; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY area_annotation
    ADD CONSTRAINT area_annotation_fk_area FOREIGN KEY (area) REFERENCES area(id);


--
-- Name: area_fk_type; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY area
    ADD CONSTRAINT area_fk_type FOREIGN KEY (type) REFERENCES area_type(id);


--
-- Name: area_gid_redirect_fk_new_id; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY area_gid_redirect
    ADD CONSTRAINT area_gid_redirect_fk_new_id FOREIGN KEY (new_id) REFERENCES area(id);


--
-- Name: artist_alias_fk_artist; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_alias
    ADD CONSTRAINT artist_alias_fk_artist FOREIGN KEY (artist) REFERENCES artist(id);


--
-- Name: artist_alias_fk_type; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_alias
    ADD CONSTRAINT artist_alias_fk_type FOREIGN KEY (type) REFERENCES artist_alias_type(id);


--
-- Name: artist_annotation_fk_annotation; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_annotation
    ADD CONSTRAINT artist_annotation_fk_annotation FOREIGN KEY (annotation) REFERENCES annotation(id);


--
-- Name: artist_annotation_fk_artist; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_annotation
    ADD CONSTRAINT artist_annotation_fk_artist FOREIGN KEY (artist) REFERENCES artist(id);


--
-- Name: artist_credit_name_fk_artist; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_credit_name
    ADD CONSTRAINT artist_credit_name_fk_artist FOREIGN KEY (artist) REFERENCES artist(id) ON DELETE CASCADE;


--
-- Name: artist_credit_name_fk_artist_credit; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_credit_name
    ADD CONSTRAINT artist_credit_name_fk_artist_credit FOREIGN KEY (artist_credit) REFERENCES artist_credit(id) ON DELETE CASCADE;


--
-- Name: artist_fk_area; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist
    ADD CONSTRAINT artist_fk_area FOREIGN KEY (area) REFERENCES area(id);


--
-- Name: artist_fk_begin_area; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist
    ADD CONSTRAINT artist_fk_begin_area FOREIGN KEY (begin_area) REFERENCES area(id);


--
-- Name: artist_fk_end_area; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist
    ADD CONSTRAINT artist_fk_end_area FOREIGN KEY (end_area) REFERENCES area(id);


--
-- Name: artist_fk_gender; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist
    ADD CONSTRAINT artist_fk_gender FOREIGN KEY (gender) REFERENCES gender(id);


--
-- Name: artist_fk_type; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist
    ADD CONSTRAINT artist_fk_type FOREIGN KEY (type) REFERENCES artist_type(id);


--
-- Name: artist_gid_redirect_fk_new_id; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_gid_redirect
    ADD CONSTRAINT artist_gid_redirect_fk_new_id FOREIGN KEY (new_id) REFERENCES artist(id);


--
-- Name: artist_ipi_fk_artist; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_ipi
    ADD CONSTRAINT artist_ipi_fk_artist FOREIGN KEY (artist) REFERENCES artist(id);


--
-- Name: artist_isni_fk_artist; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_isni
    ADD CONSTRAINT artist_isni_fk_artist FOREIGN KEY (artist) REFERENCES artist(id);


--
-- Name: artist_meta_fk_id; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_meta
    ADD CONSTRAINT artist_meta_fk_id FOREIGN KEY (id) REFERENCES artist(id) ON DELETE CASCADE;


--
-- Name: artist_rating_raw_fk_artist; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_rating_raw
    ADD CONSTRAINT artist_rating_raw_fk_artist FOREIGN KEY (artist) REFERENCES artist(id);


--
-- Name: artist_rating_raw_fk_editor; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_rating_raw
    ADD CONSTRAINT artist_rating_raw_fk_editor FOREIGN KEY (editor) REFERENCES editor(id);


--
-- Name: artist_tag_fk_artist; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_tag
    ADD CONSTRAINT artist_tag_fk_artist FOREIGN KEY (artist) REFERENCES artist(id);


--
-- Name: artist_tag_fk_tag; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_tag
    ADD CONSTRAINT artist_tag_fk_tag FOREIGN KEY (tag) REFERENCES tag(id);


--
-- Name: artist_tag_raw_fk_artist; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_tag_raw
    ADD CONSTRAINT artist_tag_raw_fk_artist FOREIGN KEY (artist) REFERENCES artist(id);


--
-- Name: artist_tag_raw_fk_editor; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_tag_raw
    ADD CONSTRAINT artist_tag_raw_fk_editor FOREIGN KEY (editor) REFERENCES editor(id);


--
-- Name: artist_tag_raw_fk_tag; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY artist_tag_raw
    ADD CONSTRAINT artist_tag_raw_fk_tag FOREIGN KEY (tag) REFERENCES tag(id);


--
-- Name: autoeditor_election_fk_candidate; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY autoeditor_election
    ADD CONSTRAINT autoeditor_election_fk_candidate FOREIGN KEY (candidate) REFERENCES editor(id);


--
-- Name: autoeditor_election_fk_proposer; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY autoeditor_election
    ADD CONSTRAINT autoeditor_election_fk_proposer FOREIGN KEY (proposer) REFERENCES editor(id);


--
-- Name: autoeditor_election_fk_seconder_1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY autoeditor_election
    ADD CONSTRAINT autoeditor_election_fk_seconder_1 FOREIGN KEY (seconder_1) REFERENCES editor(id);


--
-- Name: autoeditor_election_fk_seconder_2; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY autoeditor_election
    ADD CONSTRAINT autoeditor_election_fk_seconder_2 FOREIGN KEY (seconder_2) REFERENCES editor(id);


--
-- Name: autoeditor_election_vote_fk_autoeditor_election; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY autoeditor_election_vote
    ADD CONSTRAINT autoeditor_election_vote_fk_autoeditor_election FOREIGN KEY (autoeditor_election) REFERENCES autoeditor_election(id);


--
-- Name: autoeditor_election_vote_fk_voter; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY autoeditor_election_vote
    ADD CONSTRAINT autoeditor_election_vote_fk_voter FOREIGN KEY (voter) REFERENCES editor(id);


--
-- Name: cdtoc_raw_fk_release; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY cdtoc_raw
    ADD CONSTRAINT cdtoc_raw_fk_release FOREIGN KEY (release) REFERENCES release_raw(id);


--
-- Name: country_area_fk_area; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY country_area
    ADD CONSTRAINT country_area_fk_area FOREIGN KEY (area) REFERENCES area(id);


--
-- Name: edit_area_fk_area; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY edit_area
    ADD CONSTRAINT edit_area_fk_area FOREIGN KEY (area) REFERENCES area(id) ON DELETE CASCADE;


--
-- Name: edit_area_fk_edit; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY edit_area
    ADD CONSTRAINT edit_area_fk_edit FOREIGN KEY (edit) REFERENCES edit(id);


--
-- Name: edit_artist_fk_artist; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY edit_artist
    ADD CONSTRAINT edit_artist_fk_artist FOREIGN KEY (artist) REFERENCES artist(id) ON DELETE CASCADE;


--
-- Name: edit_artist_fk_edit; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY edit_artist
    ADD CONSTRAINT edit_artist_fk_edit FOREIGN KEY (edit) REFERENCES edit(id);


--
-- Name: edit_fk_editor; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY edit
    ADD CONSTRAINT edit_fk_editor FOREIGN KEY (editor) REFERENCES editor(id);


--
-- Name: edit_label_fk_edit; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY edit_label
    ADD CONSTRAINT edit_label_fk_edit FOREIGN KEY (edit) REFERENCES edit(id);


--
-- Name: edit_label_fk_label; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY edit_label
    ADD CONSTRAINT edit_label_fk_label FOREIGN KEY (label) REFERENCES label(id) ON DELETE CASCADE;


--
-- Name: edit_note_fk_edit; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY edit_note
    ADD CONSTRAINT edit_note_fk_edit FOREIGN KEY (edit) REFERENCES edit(id);


--
-- Name: edit_note_fk_editor; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY edit_note
    ADD CONSTRAINT edit_note_fk_editor FOREIGN KEY (editor) REFERENCES editor(id);


--
-- Name: edit_recording_fk_edit; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY edit_recording
    ADD CONSTRAINT edit_recording_fk_edit FOREIGN KEY (edit) REFERENCES edit(id);


--
-- Name: edit_recording_fk_recording; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY edit_recording
    ADD CONSTRAINT edit_recording_fk_recording FOREIGN KEY (recording) REFERENCES recording(id) ON DELETE CASCADE;


--
-- Name: edit_release_fk_edit; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY edit_release
    ADD CONSTRAINT edit_release_fk_edit FOREIGN KEY (edit) REFERENCES edit(id);


--
-- Name: edit_release_fk_release; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY edit_release
    ADD CONSTRAINT edit_release_fk_release FOREIGN KEY (release) REFERENCES release(id) ON DELETE CASCADE;


--
-- Name: edit_release_group_fk_edit; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY edit_release_group
    ADD CONSTRAINT edit_release_group_fk_edit FOREIGN KEY (edit) REFERENCES edit(id);


--
-- Name: edit_release_group_fk_release_group; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY edit_release_group
    ADD CONSTRAINT edit_release_group_fk_release_group FOREIGN KEY (release_group) REFERENCES release_group(id) ON DELETE CASCADE;


--
-- Name: edit_url_fk_edit; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY edit_url
    ADD CONSTRAINT edit_url_fk_edit FOREIGN KEY (edit) REFERENCES edit(id);


--
-- Name: edit_url_fk_url; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY edit_url
    ADD CONSTRAINT edit_url_fk_url FOREIGN KEY (url) REFERENCES url(id) ON DELETE CASCADE;


--
-- Name: edit_work_fk_edit; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY edit_work
    ADD CONSTRAINT edit_work_fk_edit FOREIGN KEY (edit) REFERENCES edit(id);


--
-- Name: edit_work_fk_work; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY edit_work
    ADD CONSTRAINT edit_work_fk_work FOREIGN KEY (work) REFERENCES work(id) ON DELETE CASCADE;


--
-- Name: editor_collection_fk_editor; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_collection
    ADD CONSTRAINT editor_collection_fk_editor FOREIGN KEY (editor) REFERENCES editor(id);


--
-- Name: editor_collection_release_fk_collection; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_collection_release
    ADD CONSTRAINT editor_collection_release_fk_collection FOREIGN KEY (collection) REFERENCES editor_collection(id);


--
-- Name: editor_collection_release_fk_release; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_collection_release
    ADD CONSTRAINT editor_collection_release_fk_release FOREIGN KEY (release) REFERENCES release(id);


--
-- Name: editor_fk_area; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor
    ADD CONSTRAINT editor_fk_area FOREIGN KEY (area) REFERENCES area(id);


--
-- Name: editor_fk_gender; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor
    ADD CONSTRAINT editor_fk_gender FOREIGN KEY (gender) REFERENCES gender(id);


--
-- Name: editor_language_fk_editor; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_language
    ADD CONSTRAINT editor_language_fk_editor FOREIGN KEY (editor) REFERENCES editor(id);


--
-- Name: editor_language_fk_language; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_language
    ADD CONSTRAINT editor_language_fk_language FOREIGN KEY (language) REFERENCES language(id);


--
-- Name: editor_oauth_token_fk_application; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_oauth_token
    ADD CONSTRAINT editor_oauth_token_fk_application FOREIGN KEY (application) REFERENCES application(id);


--
-- Name: editor_oauth_token_fk_editor; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_oauth_token
    ADD CONSTRAINT editor_oauth_token_fk_editor FOREIGN KEY (editor) REFERENCES editor(id);


--
-- Name: editor_preference_fk_editor; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_preference
    ADD CONSTRAINT editor_preference_fk_editor FOREIGN KEY (editor) REFERENCES editor(id);


--
-- Name: editor_subscribe_artist_deleted_fk_deleted_by; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_subscribe_artist_deleted
    ADD CONSTRAINT editor_subscribe_artist_deleted_fk_deleted_by FOREIGN KEY (deleted_by) REFERENCES edit(id);


--
-- Name: editor_subscribe_artist_deleted_fk_editor; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_subscribe_artist_deleted
    ADD CONSTRAINT editor_subscribe_artist_deleted_fk_editor FOREIGN KEY (editor) REFERENCES editor(id);


--
-- Name: editor_subscribe_artist_deleted_fk_gid; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_subscribe_artist_deleted
    ADD CONSTRAINT editor_subscribe_artist_deleted_fk_gid FOREIGN KEY (gid) REFERENCES artist_deletion(gid);


--
-- Name: editor_subscribe_artist_fk_artist; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_subscribe_artist
    ADD CONSTRAINT editor_subscribe_artist_fk_artist FOREIGN KEY (artist) REFERENCES artist(id);


--
-- Name: editor_subscribe_artist_fk_editor; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_subscribe_artist
    ADD CONSTRAINT editor_subscribe_artist_fk_editor FOREIGN KEY (editor) REFERENCES editor(id);


--
-- Name: editor_subscribe_artist_fk_last_edit_sent; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_subscribe_artist
    ADD CONSTRAINT editor_subscribe_artist_fk_last_edit_sent FOREIGN KEY (last_edit_sent) REFERENCES edit(id);


--
-- Name: editor_subscribe_collection_fk_editor; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_subscribe_collection
    ADD CONSTRAINT editor_subscribe_collection_fk_editor FOREIGN KEY (editor) REFERENCES editor(id);


--
-- Name: editor_subscribe_editor_fk_editor; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_subscribe_editor
    ADD CONSTRAINT editor_subscribe_editor_fk_editor FOREIGN KEY (editor) REFERENCES editor(id);


--
-- Name: editor_subscribe_editor_fk_subscribed_editor; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_subscribe_editor
    ADD CONSTRAINT editor_subscribe_editor_fk_subscribed_editor FOREIGN KEY (subscribed_editor) REFERENCES editor(id);


--
-- Name: editor_subscribe_label_deleted_fk_deleted_by; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_subscribe_label_deleted
    ADD CONSTRAINT editor_subscribe_label_deleted_fk_deleted_by FOREIGN KEY (deleted_by) REFERENCES edit(id);


--
-- Name: editor_subscribe_label_deleted_fk_editor; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_subscribe_label_deleted
    ADD CONSTRAINT editor_subscribe_label_deleted_fk_editor FOREIGN KEY (editor) REFERENCES editor(id);


--
-- Name: editor_subscribe_label_deleted_fk_gid; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_subscribe_label_deleted
    ADD CONSTRAINT editor_subscribe_label_deleted_fk_gid FOREIGN KEY (gid) REFERENCES label_deletion(gid);


--
-- Name: editor_subscribe_label_fk_editor; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_subscribe_label
    ADD CONSTRAINT editor_subscribe_label_fk_editor FOREIGN KEY (editor) REFERENCES editor(id);


--
-- Name: editor_subscribe_label_fk_label; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_subscribe_label
    ADD CONSTRAINT editor_subscribe_label_fk_label FOREIGN KEY (label) REFERENCES label(id);


--
-- Name: editor_subscribe_label_fk_last_edit_sent; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_subscribe_label
    ADD CONSTRAINT editor_subscribe_label_fk_last_edit_sent FOREIGN KEY (last_edit_sent) REFERENCES edit(id);


--
-- Name: editor_watch_artist_fk_artist; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_watch_artist
    ADD CONSTRAINT editor_watch_artist_fk_artist FOREIGN KEY (artist) REFERENCES artist(id) ON DELETE CASCADE;


--
-- Name: editor_watch_artist_fk_editor; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_watch_artist
    ADD CONSTRAINT editor_watch_artist_fk_editor FOREIGN KEY (editor) REFERENCES editor(id) ON DELETE CASCADE;


--
-- Name: editor_watch_preferences_fk_editor; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_watch_preferences
    ADD CONSTRAINT editor_watch_preferences_fk_editor FOREIGN KEY (editor) REFERENCES editor(id) ON DELETE CASCADE;


--
-- Name: editor_watch_release_group_type_fk_editor; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_watch_release_group_type
    ADD CONSTRAINT editor_watch_release_group_type_fk_editor FOREIGN KEY (editor) REFERENCES editor(id) ON DELETE CASCADE;


--
-- Name: editor_watch_release_group_type_fk_release_group_type; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_watch_release_group_type
    ADD CONSTRAINT editor_watch_release_group_type_fk_release_group_type FOREIGN KEY (release_group_type) REFERENCES release_group_primary_type(id);


--
-- Name: editor_watch_release_status_fk_editor; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_watch_release_status
    ADD CONSTRAINT editor_watch_release_status_fk_editor FOREIGN KEY (editor) REFERENCES editor(id) ON DELETE CASCADE;


--
-- Name: editor_watch_release_status_fk_release_status; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY editor_watch_release_status
    ADD CONSTRAINT editor_watch_release_status_fk_release_status FOREIGN KEY (release_status) REFERENCES release_status(id);


--
-- Name: iso_3166_1_fk_area; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY iso_3166_1
    ADD CONSTRAINT iso_3166_1_fk_area FOREIGN KEY (area) REFERENCES area(id);


--
-- Name: iso_3166_2_fk_area; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY iso_3166_2
    ADD CONSTRAINT iso_3166_2_fk_area FOREIGN KEY (area) REFERENCES area(id);


--
-- Name: iso_3166_3_fk_area; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY iso_3166_3
    ADD CONSTRAINT iso_3166_3_fk_area FOREIGN KEY (area) REFERENCES area(id);


--
-- Name: isrc_fk_recording; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY isrc
    ADD CONSTRAINT isrc_fk_recording FOREIGN KEY (recording) REFERENCES recording(id);


--
-- Name: iswc_fk_work; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY iswc
    ADD CONSTRAINT iswc_fk_work FOREIGN KEY (work) REFERENCES work(id);


--
-- Name: l_area_area_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_area
    ADD CONSTRAINT l_area_area_fk_entity0 FOREIGN KEY (entity0) REFERENCES area(id);


--
-- Name: l_area_area_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_area
    ADD CONSTRAINT l_area_area_fk_entity1 FOREIGN KEY (entity1) REFERENCES area(id);


--
-- Name: l_area_area_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_area
    ADD CONSTRAINT l_area_area_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_area_artist_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_artist
    ADD CONSTRAINT l_area_artist_fk_entity0 FOREIGN KEY (entity0) REFERENCES area(id);


--
-- Name: l_area_artist_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_artist
    ADD CONSTRAINT l_area_artist_fk_entity1 FOREIGN KEY (entity1) REFERENCES artist(id);


--
-- Name: l_area_artist_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_artist
    ADD CONSTRAINT l_area_artist_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_area_label_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_label
    ADD CONSTRAINT l_area_label_fk_entity0 FOREIGN KEY (entity0) REFERENCES area(id);


--
-- Name: l_area_label_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_label
    ADD CONSTRAINT l_area_label_fk_entity1 FOREIGN KEY (entity1) REFERENCES label(id);


--
-- Name: l_area_label_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_label
    ADD CONSTRAINT l_area_label_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_area_recording_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_recording
    ADD CONSTRAINT l_area_recording_fk_entity0 FOREIGN KEY (entity0) REFERENCES area(id);


--
-- Name: l_area_recording_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_recording
    ADD CONSTRAINT l_area_recording_fk_entity1 FOREIGN KEY (entity1) REFERENCES recording(id);


--
-- Name: l_area_recording_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_recording
    ADD CONSTRAINT l_area_recording_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_area_release_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_release
    ADD CONSTRAINT l_area_release_fk_entity0 FOREIGN KEY (entity0) REFERENCES area(id);


--
-- Name: l_area_release_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_release
    ADD CONSTRAINT l_area_release_fk_entity1 FOREIGN KEY (entity1) REFERENCES release(id);


--
-- Name: l_area_release_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_release
    ADD CONSTRAINT l_area_release_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_area_release_group_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_release_group
    ADD CONSTRAINT l_area_release_group_fk_entity0 FOREIGN KEY (entity0) REFERENCES area(id);


--
-- Name: l_area_release_group_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_release_group
    ADD CONSTRAINT l_area_release_group_fk_entity1 FOREIGN KEY (entity1) REFERENCES release_group(id);


--
-- Name: l_area_release_group_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_release_group
    ADD CONSTRAINT l_area_release_group_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_area_url_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_url
    ADD CONSTRAINT l_area_url_fk_entity0 FOREIGN KEY (entity0) REFERENCES area(id);


--
-- Name: l_area_url_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_url
    ADD CONSTRAINT l_area_url_fk_entity1 FOREIGN KEY (entity1) REFERENCES url(id);


--
-- Name: l_area_url_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_url
    ADD CONSTRAINT l_area_url_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_area_work_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_work
    ADD CONSTRAINT l_area_work_fk_entity0 FOREIGN KEY (entity0) REFERENCES area(id);


--
-- Name: l_area_work_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_work
    ADD CONSTRAINT l_area_work_fk_entity1 FOREIGN KEY (entity1) REFERENCES work(id);


--
-- Name: l_area_work_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_area_work
    ADD CONSTRAINT l_area_work_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_artist_artist_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_artist_artist
    ADD CONSTRAINT l_artist_artist_fk_entity0 FOREIGN KEY (entity0) REFERENCES artist(id);


--
-- Name: l_artist_artist_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_artist_artist
    ADD CONSTRAINT l_artist_artist_fk_entity1 FOREIGN KEY (entity1) REFERENCES artist(id);


--
-- Name: l_artist_artist_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_artist_artist
    ADD CONSTRAINT l_artist_artist_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_artist_label_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_artist_label
    ADD CONSTRAINT l_artist_label_fk_entity0 FOREIGN KEY (entity0) REFERENCES artist(id);


--
-- Name: l_artist_label_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_artist_label
    ADD CONSTRAINT l_artist_label_fk_entity1 FOREIGN KEY (entity1) REFERENCES label(id);


--
-- Name: l_artist_label_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_artist_label
    ADD CONSTRAINT l_artist_label_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_artist_recording_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_artist_recording
    ADD CONSTRAINT l_artist_recording_fk_entity0 FOREIGN KEY (entity0) REFERENCES artist(id);


--
-- Name: l_artist_recording_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_artist_recording
    ADD CONSTRAINT l_artist_recording_fk_entity1 FOREIGN KEY (entity1) REFERENCES recording(id);


--
-- Name: l_artist_recording_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_artist_recording
    ADD CONSTRAINT l_artist_recording_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_artist_release_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_artist_release
    ADD CONSTRAINT l_artist_release_fk_entity0 FOREIGN KEY (entity0) REFERENCES artist(id);


--
-- Name: l_artist_release_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_artist_release
    ADD CONSTRAINT l_artist_release_fk_entity1 FOREIGN KEY (entity1) REFERENCES release(id);


--
-- Name: l_artist_release_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_artist_release
    ADD CONSTRAINT l_artist_release_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_artist_release_group_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_artist_release_group
    ADD CONSTRAINT l_artist_release_group_fk_entity0 FOREIGN KEY (entity0) REFERENCES artist(id);


--
-- Name: l_artist_release_group_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_artist_release_group
    ADD CONSTRAINT l_artist_release_group_fk_entity1 FOREIGN KEY (entity1) REFERENCES release_group(id);


--
-- Name: l_artist_release_group_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_artist_release_group
    ADD CONSTRAINT l_artist_release_group_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_artist_url_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_artist_url
    ADD CONSTRAINT l_artist_url_fk_entity0 FOREIGN KEY (entity0) REFERENCES artist(id);


--
-- Name: l_artist_url_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_artist_url
    ADD CONSTRAINT l_artist_url_fk_entity1 FOREIGN KEY (entity1) REFERENCES url(id);


--
-- Name: l_artist_url_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_artist_url
    ADD CONSTRAINT l_artist_url_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_artist_work_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_artist_work
    ADD CONSTRAINT l_artist_work_fk_entity0 FOREIGN KEY (entity0) REFERENCES artist(id);


--
-- Name: l_artist_work_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_artist_work
    ADD CONSTRAINT l_artist_work_fk_entity1 FOREIGN KEY (entity1) REFERENCES work(id);


--
-- Name: l_artist_work_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_artist_work
    ADD CONSTRAINT l_artist_work_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_label_label_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_label_label
    ADD CONSTRAINT l_label_label_fk_entity0 FOREIGN KEY (entity0) REFERENCES label(id);


--
-- Name: l_label_label_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_label_label
    ADD CONSTRAINT l_label_label_fk_entity1 FOREIGN KEY (entity1) REFERENCES label(id);


--
-- Name: l_label_label_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_label_label
    ADD CONSTRAINT l_label_label_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_label_recording_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_label_recording
    ADD CONSTRAINT l_label_recording_fk_entity0 FOREIGN KEY (entity0) REFERENCES label(id);


--
-- Name: l_label_recording_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_label_recording
    ADD CONSTRAINT l_label_recording_fk_entity1 FOREIGN KEY (entity1) REFERENCES recording(id);


--
-- Name: l_label_recording_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_label_recording
    ADD CONSTRAINT l_label_recording_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_label_release_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_label_release
    ADD CONSTRAINT l_label_release_fk_entity0 FOREIGN KEY (entity0) REFERENCES label(id);


--
-- Name: l_label_release_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_label_release
    ADD CONSTRAINT l_label_release_fk_entity1 FOREIGN KEY (entity1) REFERENCES release(id);


--
-- Name: l_label_release_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_label_release
    ADD CONSTRAINT l_label_release_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_label_release_group_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_label_release_group
    ADD CONSTRAINT l_label_release_group_fk_entity0 FOREIGN KEY (entity0) REFERENCES label(id);


--
-- Name: l_label_release_group_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_label_release_group
    ADD CONSTRAINT l_label_release_group_fk_entity1 FOREIGN KEY (entity1) REFERENCES release_group(id);


--
-- Name: l_label_release_group_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_label_release_group
    ADD CONSTRAINT l_label_release_group_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_label_url_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_label_url
    ADD CONSTRAINT l_label_url_fk_entity0 FOREIGN KEY (entity0) REFERENCES label(id);


--
-- Name: l_label_url_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_label_url
    ADD CONSTRAINT l_label_url_fk_entity1 FOREIGN KEY (entity1) REFERENCES url(id);


--
-- Name: l_label_url_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_label_url
    ADD CONSTRAINT l_label_url_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_label_work_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_label_work
    ADD CONSTRAINT l_label_work_fk_entity0 FOREIGN KEY (entity0) REFERENCES label(id);


--
-- Name: l_label_work_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_label_work
    ADD CONSTRAINT l_label_work_fk_entity1 FOREIGN KEY (entity1) REFERENCES work(id);


--
-- Name: l_label_work_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_label_work
    ADD CONSTRAINT l_label_work_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_recording_recording_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_recording_recording
    ADD CONSTRAINT l_recording_recording_fk_entity0 FOREIGN KEY (entity0) REFERENCES recording(id);


--
-- Name: l_recording_recording_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_recording_recording
    ADD CONSTRAINT l_recording_recording_fk_entity1 FOREIGN KEY (entity1) REFERENCES recording(id);


--
-- Name: l_recording_recording_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_recording_recording
    ADD CONSTRAINT l_recording_recording_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_recording_release_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_recording_release
    ADD CONSTRAINT l_recording_release_fk_entity0 FOREIGN KEY (entity0) REFERENCES recording(id);


--
-- Name: l_recording_release_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_recording_release
    ADD CONSTRAINT l_recording_release_fk_entity1 FOREIGN KEY (entity1) REFERENCES release(id);


--
-- Name: l_recording_release_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_recording_release
    ADD CONSTRAINT l_recording_release_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_recording_release_group_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_recording_release_group
    ADD CONSTRAINT l_recording_release_group_fk_entity0 FOREIGN KEY (entity0) REFERENCES recording(id);


--
-- Name: l_recording_release_group_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_recording_release_group
    ADD CONSTRAINT l_recording_release_group_fk_entity1 FOREIGN KEY (entity1) REFERENCES release_group(id);


--
-- Name: l_recording_release_group_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_recording_release_group
    ADD CONSTRAINT l_recording_release_group_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_recording_url_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_recording_url
    ADD CONSTRAINT l_recording_url_fk_entity0 FOREIGN KEY (entity0) REFERENCES recording(id);


--
-- Name: l_recording_url_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_recording_url
    ADD CONSTRAINT l_recording_url_fk_entity1 FOREIGN KEY (entity1) REFERENCES url(id);


--
-- Name: l_recording_url_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_recording_url
    ADD CONSTRAINT l_recording_url_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_recording_work_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_recording_work
    ADD CONSTRAINT l_recording_work_fk_entity0 FOREIGN KEY (entity0) REFERENCES recording(id);


--
-- Name: l_recording_work_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_recording_work
    ADD CONSTRAINT l_recording_work_fk_entity1 FOREIGN KEY (entity1) REFERENCES work(id);


--
-- Name: l_recording_work_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_recording_work
    ADD CONSTRAINT l_recording_work_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_release_group_release_group_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_release_group_release_group
    ADD CONSTRAINT l_release_group_release_group_fk_entity0 FOREIGN KEY (entity0) REFERENCES release_group(id);


--
-- Name: l_release_group_release_group_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_release_group_release_group
    ADD CONSTRAINT l_release_group_release_group_fk_entity1 FOREIGN KEY (entity1) REFERENCES release_group(id);


--
-- Name: l_release_group_release_group_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_release_group_release_group
    ADD CONSTRAINT l_release_group_release_group_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_release_group_url_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_release_group_url
    ADD CONSTRAINT l_release_group_url_fk_entity0 FOREIGN KEY (entity0) REFERENCES release_group(id);


--
-- Name: l_release_group_url_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_release_group_url
    ADD CONSTRAINT l_release_group_url_fk_entity1 FOREIGN KEY (entity1) REFERENCES url(id);


--
-- Name: l_release_group_url_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_release_group_url
    ADD CONSTRAINT l_release_group_url_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_release_group_work_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_release_group_work
    ADD CONSTRAINT l_release_group_work_fk_entity0 FOREIGN KEY (entity0) REFERENCES release_group(id);


--
-- Name: l_release_group_work_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_release_group_work
    ADD CONSTRAINT l_release_group_work_fk_entity1 FOREIGN KEY (entity1) REFERENCES work(id);


--
-- Name: l_release_group_work_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_release_group_work
    ADD CONSTRAINT l_release_group_work_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_release_release_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_release_release
    ADD CONSTRAINT l_release_release_fk_entity0 FOREIGN KEY (entity0) REFERENCES release(id);


--
-- Name: l_release_release_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_release_release
    ADD CONSTRAINT l_release_release_fk_entity1 FOREIGN KEY (entity1) REFERENCES release(id);


--
-- Name: l_release_release_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_release_release
    ADD CONSTRAINT l_release_release_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_release_release_group_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_release_release_group
    ADD CONSTRAINT l_release_release_group_fk_entity0 FOREIGN KEY (entity0) REFERENCES release(id);


--
-- Name: l_release_release_group_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_release_release_group
    ADD CONSTRAINT l_release_release_group_fk_entity1 FOREIGN KEY (entity1) REFERENCES release_group(id);


--
-- Name: l_release_release_group_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_release_release_group
    ADD CONSTRAINT l_release_release_group_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_release_url_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_release_url
    ADD CONSTRAINT l_release_url_fk_entity0 FOREIGN KEY (entity0) REFERENCES release(id);


--
-- Name: l_release_url_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_release_url
    ADD CONSTRAINT l_release_url_fk_entity1 FOREIGN KEY (entity1) REFERENCES url(id);


--
-- Name: l_release_url_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_release_url
    ADD CONSTRAINT l_release_url_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_release_work_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_release_work
    ADD CONSTRAINT l_release_work_fk_entity0 FOREIGN KEY (entity0) REFERENCES release(id);


--
-- Name: l_release_work_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_release_work
    ADD CONSTRAINT l_release_work_fk_entity1 FOREIGN KEY (entity1) REFERENCES work(id);


--
-- Name: l_release_work_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_release_work
    ADD CONSTRAINT l_release_work_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_url_url_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_url_url
    ADD CONSTRAINT l_url_url_fk_entity0 FOREIGN KEY (entity0) REFERENCES url(id);


--
-- Name: l_url_url_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_url_url
    ADD CONSTRAINT l_url_url_fk_entity1 FOREIGN KEY (entity1) REFERENCES url(id);


--
-- Name: l_url_url_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_url_url
    ADD CONSTRAINT l_url_url_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_url_work_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_url_work
    ADD CONSTRAINT l_url_work_fk_entity0 FOREIGN KEY (entity0) REFERENCES url(id);


--
-- Name: l_url_work_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_url_work
    ADD CONSTRAINT l_url_work_fk_entity1 FOREIGN KEY (entity1) REFERENCES work(id);


--
-- Name: l_url_work_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_url_work
    ADD CONSTRAINT l_url_work_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: l_work_work_fk_entity0; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_work_work
    ADD CONSTRAINT l_work_work_fk_entity0 FOREIGN KEY (entity0) REFERENCES work(id);


--
-- Name: l_work_work_fk_entity1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_work_work
    ADD CONSTRAINT l_work_work_fk_entity1 FOREIGN KEY (entity1) REFERENCES work(id);


--
-- Name: l_work_work_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY l_work_work
    ADD CONSTRAINT l_work_work_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: label_alias_fk_label; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_alias
    ADD CONSTRAINT label_alias_fk_label FOREIGN KEY (label) REFERENCES label(id);


--
-- Name: label_alias_fk_type; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_alias
    ADD CONSTRAINT label_alias_fk_type FOREIGN KEY (type) REFERENCES label_alias_type(id);


--
-- Name: label_annotation_fk_annotation; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_annotation
    ADD CONSTRAINT label_annotation_fk_annotation FOREIGN KEY (annotation) REFERENCES annotation(id);


--
-- Name: label_annotation_fk_label; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_annotation
    ADD CONSTRAINT label_annotation_fk_label FOREIGN KEY (label) REFERENCES label(id);


--
-- Name: label_fk_area; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label
    ADD CONSTRAINT label_fk_area FOREIGN KEY (area) REFERENCES area(id);


--
-- Name: label_fk_type; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label
    ADD CONSTRAINT label_fk_type FOREIGN KEY (type) REFERENCES label_type(id);


--
-- Name: label_gid_redirect_fk_new_id; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_gid_redirect
    ADD CONSTRAINT label_gid_redirect_fk_new_id FOREIGN KEY (new_id) REFERENCES label(id);


--
-- Name: label_ipi_fk_label; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_ipi
    ADD CONSTRAINT label_ipi_fk_label FOREIGN KEY (label) REFERENCES label(id);


--
-- Name: label_isni_fk_label; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_isni
    ADD CONSTRAINT label_isni_fk_label FOREIGN KEY (label) REFERENCES label(id);


--
-- Name: label_meta_fk_id; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_meta
    ADD CONSTRAINT label_meta_fk_id FOREIGN KEY (id) REFERENCES label(id) ON DELETE CASCADE;


--
-- Name: label_rating_raw_fk_editor; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_rating_raw
    ADD CONSTRAINT label_rating_raw_fk_editor FOREIGN KEY (editor) REFERENCES editor(id);


--
-- Name: label_rating_raw_fk_label; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_rating_raw
    ADD CONSTRAINT label_rating_raw_fk_label FOREIGN KEY (label) REFERENCES label(id);


--
-- Name: label_tag_fk_label; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_tag
    ADD CONSTRAINT label_tag_fk_label FOREIGN KEY (label) REFERENCES label(id);


--
-- Name: label_tag_fk_tag; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_tag
    ADD CONSTRAINT label_tag_fk_tag FOREIGN KEY (tag) REFERENCES tag(id);


--
-- Name: label_tag_raw_fk_editor; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_tag_raw
    ADD CONSTRAINT label_tag_raw_fk_editor FOREIGN KEY (editor) REFERENCES editor(id);


--
-- Name: label_tag_raw_fk_label; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_tag_raw
    ADD CONSTRAINT label_tag_raw_fk_label FOREIGN KEY (label) REFERENCES label(id);


--
-- Name: label_tag_raw_fk_tag; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY label_tag_raw
    ADD CONSTRAINT label_tag_raw_fk_tag FOREIGN KEY (tag) REFERENCES tag(id);


--
-- Name: link_attribute_credit_fk_attribute_type; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY link_attribute_credit
    ADD CONSTRAINT link_attribute_credit_fk_attribute_type FOREIGN KEY (attribute_type) REFERENCES link_creditable_attribute_type(attribute_type);


--
-- Name: link_attribute_credit_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY link_attribute_credit
    ADD CONSTRAINT link_attribute_credit_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: link_attribute_fk_attribute_type; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY link_attribute
    ADD CONSTRAINT link_attribute_fk_attribute_type FOREIGN KEY (attribute_type) REFERENCES link_attribute_type(id);


--
-- Name: link_attribute_fk_link; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY link_attribute
    ADD CONSTRAINT link_attribute_fk_link FOREIGN KEY (link) REFERENCES link(id);


--
-- Name: link_attribute_type_fk_parent; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY link_attribute_type
    ADD CONSTRAINT link_attribute_type_fk_parent FOREIGN KEY (parent) REFERENCES link_attribute_type(id);


--
-- Name: link_attribute_type_fk_root; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY link_attribute_type
    ADD CONSTRAINT link_attribute_type_fk_root FOREIGN KEY (root) REFERENCES link_attribute_type(id);


--
-- Name: link_creditable_attribute_type_fk_attribute_type; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY link_creditable_attribute_type
    ADD CONSTRAINT link_creditable_attribute_type_fk_attribute_type FOREIGN KEY (attribute_type) REFERENCES link_attribute_type(id) ON DELETE CASCADE;


--
-- Name: link_fk_link_type; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY link
    ADD CONSTRAINT link_fk_link_type FOREIGN KEY (link_type) REFERENCES link_type(id);


--
-- Name: link_type_attribute_type_fk_attribute_type; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY link_type_attribute_type
    ADD CONSTRAINT link_type_attribute_type_fk_attribute_type FOREIGN KEY (attribute_type) REFERENCES link_attribute_type(id);


--
-- Name: link_type_attribute_type_fk_link_type; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY link_type_attribute_type
    ADD CONSTRAINT link_type_attribute_type_fk_link_type FOREIGN KEY (link_type) REFERENCES link_type(id);


--
-- Name: link_type_fk_parent; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY link_type
    ADD CONSTRAINT link_type_fk_parent FOREIGN KEY (parent) REFERENCES link_type(id);


--
-- Name: medium_cdtoc_fk_cdtoc; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY medium_cdtoc
    ADD CONSTRAINT medium_cdtoc_fk_cdtoc FOREIGN KEY (cdtoc) REFERENCES cdtoc(id);


--
-- Name: medium_cdtoc_fk_medium; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY medium_cdtoc
    ADD CONSTRAINT medium_cdtoc_fk_medium FOREIGN KEY (medium) REFERENCES medium(id);


--
-- Name: medium_fk_format; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY medium
    ADD CONSTRAINT medium_fk_format FOREIGN KEY (format) REFERENCES medium_format(id);


--
-- Name: medium_fk_release; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY medium
    ADD CONSTRAINT medium_fk_release FOREIGN KEY (release) REFERENCES release(id);


--
-- Name: medium_format_fk_parent; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY medium_format
    ADD CONSTRAINT medium_format_fk_parent FOREIGN KEY (parent) REFERENCES medium_format(id);


--
-- Name: medium_index_fk_medium; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY medium_index
    ADD CONSTRAINT medium_index_fk_medium FOREIGN KEY (medium) REFERENCES medium(id) ON DELETE CASCADE;


--
-- Name: recording_annotation_fk_annotation; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY recording_annotation
    ADD CONSTRAINT recording_annotation_fk_annotation FOREIGN KEY (annotation) REFERENCES annotation(id);


--
-- Name: recording_annotation_fk_recording; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY recording_annotation
    ADD CONSTRAINT recording_annotation_fk_recording FOREIGN KEY (recording) REFERENCES recording(id);


--
-- Name: recording_fk_artist_credit; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY recording
    ADD CONSTRAINT recording_fk_artist_credit FOREIGN KEY (artist_credit) REFERENCES artist_credit(id);


--
-- Name: recording_gid_redirect_fk_new_id; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY recording_gid_redirect
    ADD CONSTRAINT recording_gid_redirect_fk_new_id FOREIGN KEY (new_id) REFERENCES recording(id);


--
-- Name: recording_meta_fk_id; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY recording_meta
    ADD CONSTRAINT recording_meta_fk_id FOREIGN KEY (id) REFERENCES recording(id) ON DELETE CASCADE;


--
-- Name: recording_rating_raw_fk_editor; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY recording_rating_raw
    ADD CONSTRAINT recording_rating_raw_fk_editor FOREIGN KEY (editor) REFERENCES editor(id);


--
-- Name: recording_rating_raw_fk_recording; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY recording_rating_raw
    ADD CONSTRAINT recording_rating_raw_fk_recording FOREIGN KEY (recording) REFERENCES recording(id);


--
-- Name: recording_tag_fk_recording; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY recording_tag
    ADD CONSTRAINT recording_tag_fk_recording FOREIGN KEY (recording) REFERENCES recording(id);


--
-- Name: recording_tag_fk_tag; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY recording_tag
    ADD CONSTRAINT recording_tag_fk_tag FOREIGN KEY (tag) REFERENCES tag(id);


--
-- Name: recording_tag_raw_fk_editor; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY recording_tag_raw
    ADD CONSTRAINT recording_tag_raw_fk_editor FOREIGN KEY (editor) REFERENCES editor(id);


--
-- Name: recording_tag_raw_fk_recording; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY recording_tag_raw
    ADD CONSTRAINT recording_tag_raw_fk_recording FOREIGN KEY (recording) REFERENCES recording(id);


--
-- Name: recording_tag_raw_fk_tag; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY recording_tag_raw
    ADD CONSTRAINT recording_tag_raw_fk_tag FOREIGN KEY (tag) REFERENCES tag(id);


--
-- Name: release_annotation_fk_annotation; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_annotation
    ADD CONSTRAINT release_annotation_fk_annotation FOREIGN KEY (annotation) REFERENCES annotation(id);


--
-- Name: release_annotation_fk_release; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_annotation
    ADD CONSTRAINT release_annotation_fk_release FOREIGN KEY (release) REFERENCES release(id);


--
-- Name: release_country_fk_country; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_country
    ADD CONSTRAINT release_country_fk_country FOREIGN KEY (country) REFERENCES country_area(area);


--
-- Name: release_country_fk_release; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_country
    ADD CONSTRAINT release_country_fk_release FOREIGN KEY (release) REFERENCES release(id);


--
-- Name: release_coverart_fk_id; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_coverart
    ADD CONSTRAINT release_coverart_fk_id FOREIGN KEY (id) REFERENCES release(id) ON DELETE CASCADE;


--
-- Name: release_fk_artist_credit; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release
    ADD CONSTRAINT release_fk_artist_credit FOREIGN KEY (artist_credit) REFERENCES artist_credit(id);


--
-- Name: release_fk_language; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release
    ADD CONSTRAINT release_fk_language FOREIGN KEY (language) REFERENCES language(id);


--
-- Name: release_fk_packaging; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release
    ADD CONSTRAINT release_fk_packaging FOREIGN KEY (packaging) REFERENCES release_packaging(id);


--
-- Name: release_fk_release_group; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release
    ADD CONSTRAINT release_fk_release_group FOREIGN KEY (release_group) REFERENCES release_group(id);


--
-- Name: release_fk_script; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release
    ADD CONSTRAINT release_fk_script FOREIGN KEY (script) REFERENCES script(id);


--
-- Name: release_fk_status; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release
    ADD CONSTRAINT release_fk_status FOREIGN KEY (status) REFERENCES release_status(id);


--
-- Name: release_gid_redirect_fk_new_id; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_gid_redirect
    ADD CONSTRAINT release_gid_redirect_fk_new_id FOREIGN KEY (new_id) REFERENCES release(id);


--
-- Name: release_group_annotation_fk_annotation; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group_annotation
    ADD CONSTRAINT release_group_annotation_fk_annotation FOREIGN KEY (annotation) REFERENCES annotation(id);


--
-- Name: release_group_annotation_fk_release_group; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group_annotation
    ADD CONSTRAINT release_group_annotation_fk_release_group FOREIGN KEY (release_group) REFERENCES release_group(id);


--
-- Name: release_group_fk_artist_credit; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group
    ADD CONSTRAINT release_group_fk_artist_credit FOREIGN KEY (artist_credit) REFERENCES artist_credit(id);


--
-- Name: release_group_fk_type; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group
    ADD CONSTRAINT release_group_fk_type FOREIGN KEY (type) REFERENCES release_group_primary_type(id);


--
-- Name: release_group_gid_redirect_fk_new_id; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group_gid_redirect
    ADD CONSTRAINT release_group_gid_redirect_fk_new_id FOREIGN KEY (new_id) REFERENCES release_group(id);


--
-- Name: release_group_meta_fk_id; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group_meta
    ADD CONSTRAINT release_group_meta_fk_id FOREIGN KEY (id) REFERENCES release_group(id) ON DELETE CASCADE;


--
-- Name: release_group_rating_raw_fk_editor; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group_rating_raw
    ADD CONSTRAINT release_group_rating_raw_fk_editor FOREIGN KEY (editor) REFERENCES editor(id);


--
-- Name: release_group_rating_raw_fk_release_group; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group_rating_raw
    ADD CONSTRAINT release_group_rating_raw_fk_release_group FOREIGN KEY (release_group) REFERENCES release_group(id);


--
-- Name: release_group_secondary_type_join_fk_release_group; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group_secondary_type_join
    ADD CONSTRAINT release_group_secondary_type_join_fk_release_group FOREIGN KEY (release_group) REFERENCES release_group(id);


--
-- Name: release_group_secondary_type_join_fk_secondary_type; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group_secondary_type_join
    ADD CONSTRAINT release_group_secondary_type_join_fk_secondary_type FOREIGN KEY (secondary_type) REFERENCES release_group_secondary_type(id);


--
-- Name: release_group_tag_fk_release_group; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group_tag
    ADD CONSTRAINT release_group_tag_fk_release_group FOREIGN KEY (release_group) REFERENCES release_group(id);


--
-- Name: release_group_tag_fk_tag; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group_tag
    ADD CONSTRAINT release_group_tag_fk_tag FOREIGN KEY (tag) REFERENCES tag(id);


--
-- Name: release_group_tag_raw_fk_editor; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group_tag_raw
    ADD CONSTRAINT release_group_tag_raw_fk_editor FOREIGN KEY (editor) REFERENCES editor(id);


--
-- Name: release_group_tag_raw_fk_release_group; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group_tag_raw
    ADD CONSTRAINT release_group_tag_raw_fk_release_group FOREIGN KEY (release_group) REFERENCES release_group(id);


--
-- Name: release_group_tag_raw_fk_tag; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_group_tag_raw
    ADD CONSTRAINT release_group_tag_raw_fk_tag FOREIGN KEY (tag) REFERENCES tag(id);


--
-- Name: release_label_fk_label; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_label
    ADD CONSTRAINT release_label_fk_label FOREIGN KEY (label) REFERENCES label(id);


--
-- Name: release_label_fk_release; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_label
    ADD CONSTRAINT release_label_fk_release FOREIGN KEY (release) REFERENCES release(id);


--
-- Name: release_meta_fk_id; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_meta
    ADD CONSTRAINT release_meta_fk_id FOREIGN KEY (id) REFERENCES release(id) ON DELETE CASCADE;


--
-- Name: release_tag_fk_release; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_tag
    ADD CONSTRAINT release_tag_fk_release FOREIGN KEY (release) REFERENCES release(id);


--
-- Name: release_tag_fk_tag; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_tag
    ADD CONSTRAINT release_tag_fk_tag FOREIGN KEY (tag) REFERENCES tag(id);


--
-- Name: release_tag_raw_fk_editor; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_tag_raw
    ADD CONSTRAINT release_tag_raw_fk_editor FOREIGN KEY (editor) REFERENCES editor(id);


--
-- Name: release_tag_raw_fk_release; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_tag_raw
    ADD CONSTRAINT release_tag_raw_fk_release FOREIGN KEY (release) REFERENCES release(id);


--
-- Name: release_tag_raw_fk_tag; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_tag_raw
    ADD CONSTRAINT release_tag_raw_fk_tag FOREIGN KEY (tag) REFERENCES tag(id);


--
-- Name: release_unknown_country_fk_release; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY release_unknown_country
    ADD CONSTRAINT release_unknown_country_fk_release FOREIGN KEY (release) REFERENCES release(id);


--
-- Name: script_language_fk_language; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY script_language
    ADD CONSTRAINT script_language_fk_language FOREIGN KEY (language) REFERENCES language(id);


--
-- Name: script_language_fk_script; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY script_language
    ADD CONSTRAINT script_language_fk_script FOREIGN KEY (script) REFERENCES script(id);


--
-- Name: tag_relation_fk_tag1; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY tag_relation
    ADD CONSTRAINT tag_relation_fk_tag1 FOREIGN KEY (tag1) REFERENCES tag(id);


--
-- Name: tag_relation_fk_tag2; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY tag_relation
    ADD CONSTRAINT tag_relation_fk_tag2 FOREIGN KEY (tag2) REFERENCES tag(id);


--
-- Name: track_fk_artist_credit; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY track
    ADD CONSTRAINT track_fk_artist_credit FOREIGN KEY (artist_credit) REFERENCES artist_credit(id);


--
-- Name: track_fk_medium; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY track
    ADD CONSTRAINT track_fk_medium FOREIGN KEY (medium) REFERENCES medium(id);


--
-- Name: track_fk_recording; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY track
    ADD CONSTRAINT track_fk_recording FOREIGN KEY (recording) REFERENCES recording(id);


--
-- Name: track_gid_redirect_fk_new_id; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY track_gid_redirect
    ADD CONSTRAINT track_gid_redirect_fk_new_id FOREIGN KEY (new_id) REFERENCES track(id);


--
-- Name: track_raw_fk_release; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY track_raw
    ADD CONSTRAINT track_raw_fk_release FOREIGN KEY (release) REFERENCES release_raw(id);


--
-- Name: url_gid_redirect_fk_new_id; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY url_gid_redirect
    ADD CONSTRAINT url_gid_redirect_fk_new_id FOREIGN KEY (new_id) REFERENCES url(id);


--
-- Name: vote_fk_edit; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY vote
    ADD CONSTRAINT vote_fk_edit FOREIGN KEY (edit) REFERENCES edit(id);


--
-- Name: vote_fk_editor; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY vote
    ADD CONSTRAINT vote_fk_editor FOREIGN KEY (editor) REFERENCES editor(id);


--
-- Name: work_alias_fk_type; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_alias
    ADD CONSTRAINT work_alias_fk_type FOREIGN KEY (type) REFERENCES work_alias_type(id);


--
-- Name: work_alias_fk_work; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_alias
    ADD CONSTRAINT work_alias_fk_work FOREIGN KEY (work) REFERENCES work(id);


--
-- Name: work_annotation_fk_annotation; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_annotation
    ADD CONSTRAINT work_annotation_fk_annotation FOREIGN KEY (annotation) REFERENCES annotation(id);


--
-- Name: work_annotation_fk_work; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_annotation
    ADD CONSTRAINT work_annotation_fk_work FOREIGN KEY (work) REFERENCES work(id);


--
-- Name: work_attribute_fk_work; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_attribute
    ADD CONSTRAINT work_attribute_fk_work FOREIGN KEY (work) REFERENCES work(id);


--
-- Name: work_attribute_fk_work_attribute_type; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_attribute
    ADD CONSTRAINT work_attribute_fk_work_attribute_type FOREIGN KEY (work_attribute_type) REFERENCES work_attribute_type(id);


--
-- Name: work_attribute_fk_work_attribute_type_allowed_value; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_attribute
    ADD CONSTRAINT work_attribute_fk_work_attribute_type_allowed_value FOREIGN KEY (work_attribute_type_allowed_value) REFERENCES work_attribute_type_allowed_value(id);


--
-- Name: work_attribute_type_allowed_value_fk_work_attribute_type; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_attribute_type_allowed_value
    ADD CONSTRAINT work_attribute_type_allowed_value_fk_work_attribute_type FOREIGN KEY (work_attribute_type) REFERENCES work_attribute_type(id);


--
-- Name: work_fk_language; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work
    ADD CONSTRAINT work_fk_language FOREIGN KEY (language) REFERENCES language(id);


--
-- Name: work_fk_type; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work
    ADD CONSTRAINT work_fk_type FOREIGN KEY (type) REFERENCES work_type(id);


--
-- Name: work_gid_redirect_fk_new_id; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_gid_redirect
    ADD CONSTRAINT work_gid_redirect_fk_new_id FOREIGN KEY (new_id) REFERENCES work(id);


--
-- Name: work_meta_fk_id; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_meta
    ADD CONSTRAINT work_meta_fk_id FOREIGN KEY (id) REFERENCES work(id) ON DELETE CASCADE;


--
-- Name: work_rating_raw_fk_editor; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_rating_raw
    ADD CONSTRAINT work_rating_raw_fk_editor FOREIGN KEY (editor) REFERENCES editor(id);


--
-- Name: work_rating_raw_fk_work; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_rating_raw
    ADD CONSTRAINT work_rating_raw_fk_work FOREIGN KEY (work) REFERENCES work(id);


--
-- Name: work_tag_fk_tag; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_tag
    ADD CONSTRAINT work_tag_fk_tag FOREIGN KEY (tag) REFERENCES tag(id);


--
-- Name: work_tag_fk_work; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_tag
    ADD CONSTRAINT work_tag_fk_work FOREIGN KEY (work) REFERENCES work(id);


--
-- Name: work_tag_raw_fk_editor; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_tag_raw
    ADD CONSTRAINT work_tag_raw_fk_editor FOREIGN KEY (editor) REFERENCES editor(id);


--
-- Name: work_tag_raw_fk_tag; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_tag_raw
    ADD CONSTRAINT work_tag_raw_fk_tag FOREIGN KEY (tag) REFERENCES tag(id);


--
-- Name: work_tag_raw_fk_work; Type: FK CONSTRAINT; Schema: musicbrainz; Owner: musicbrainz
--

ALTER TABLE ONLY work_tag_raw
    ADD CONSTRAINT work_tag_raw_fk_work FOREIGN KEY (work) REFERENCES work(id);


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

