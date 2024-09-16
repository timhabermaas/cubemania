--
-- PostgreSQL database dump
--

-- Dumped from database version 9.5.3
-- Dumped by pg_dump version 13.4

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner:
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET default_tablespace = '';

--
-- Name: activities; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.activities (
    id integer NOT NULL,
    trackable_id integer NOT NULL,
    user_id integer NOT NULL,
    type character varying(255) NOT NULL,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL,
    comments_count integer DEFAULT 0 NOT NULL
);


ALTER TABLE public.activities OWNER TO postgres;

--
-- Name: activities_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.activities_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.activities_id_seq OWNER TO postgres;

--
-- Name: activities_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.activities_id_seq OWNED BY public.activities.id;


--
-- Name: singles; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.singles (
    id integer NOT NULL,
    "time" integer NOT NULL,
    puzzle_id integer NOT NULL,
    created_at timestamp without time zone NOT NULL,
    user_id integer NOT NULL,
    scramble character varying(1024),
    comment character varying(255),
    updated_at timestamp without time zone NOT NULL,
    penalty character varying(8)
);


ALTER TABLE public.singles OWNER TO postgres;

--
-- Name: clocks_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.clocks_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.clocks_id_seq OWNER TO postgres;

--
-- Name: clocks_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.clocks_id_seq OWNED BY public.singles.id;


--
-- Name: comments; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.comments (
    id integer NOT NULL,
    content text NOT NULL,
    user_id integer,
    created_at timestamp without time zone NOT NULL,
    commentable_id integer NOT NULL,
    commentable_type character varying(255) DEFAULT NULL::character varying NOT NULL
);


ALTER TABLE public.comments OWNER TO postgres;

--
-- Name: comments_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.comments_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comments_id_seq OWNER TO postgres;

--
-- Name: comments_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.comments_id_seq OWNED BY public.comments.id;


--
-- Name: cubing_sessions; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.cubing_sessions (
    id integer NOT NULL,
    user_id integer,
    puzzle_id integer,
    single_ids text,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL
);


ALTER TABLE public.cubing_sessions OWNER TO postgres;

--
-- Name: cubing_sessions_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.cubing_sessions_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.cubing_sessions_id_seq OWNER TO postgres;

--
-- Name: cubing_sessions_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.cubing_sessions_id_seq OWNED BY public.cubing_sessions.id;


--
-- Name: delayed_jobs; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.delayed_jobs (
    id integer NOT NULL,
    priority integer DEFAULT 0,
    attempts integer DEFAULT 0,
    handler text,
    last_error text,
    run_at timestamp without time zone,
    locked_at timestamp without time zone,
    failed_at timestamp without time zone,
    locked_by character varying(255),
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL,
    queue character varying(255)
);


ALTER TABLE public.delayed_jobs OWNER TO postgres;

--
-- Name: delayed_jobs_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.delayed_jobs_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.delayed_jobs_id_seq OWNER TO postgres;

--
-- Name: delayed_jobs_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.delayed_jobs_id_seq OWNED BY public.delayed_jobs.id;

--
-- Name: followings; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.followings (
    id integer NOT NULL,
    follower_id integer NOT NULL,
    followee_id integer NOT NULL,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL
);


ALTER TABLE public.followings OWNER TO postgres;

--
-- Name: followings_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.followings_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.followings_id_seq OWNER TO postgres;

--
-- Name: followings_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.followings_id_seq OWNED BY public.followings.id;


--
-- Name: kinds; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.kinds (
    id integer NOT NULL,
    name character varying(64) NOT NULL,
    css_position integer DEFAULT 0 NOT NULL,
    short_name character varying(255)
);


ALTER TABLE public.kinds OWNER TO postgres;

--
-- Name: kinds_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.kinds_id_seq
    START WITH 5
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.kinds_id_seq OWNER TO postgres;

--
-- Name: kinds_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.kinds_id_seq OWNED BY public.kinds.id;


--
-- Name: posts; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.posts (
    id integer NOT NULL,
    title character varying(255),
    content text,
    user_id integer,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL,
    comments_count integer DEFAULT 0 NOT NULL
);


ALTER TABLE public.posts OWNER TO postgres;

--
-- Name: posts_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.posts_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.posts_id_seq OWNER TO postgres;

--
-- Name: posts_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.posts_id_seq OWNED BY public.posts.id;


--
-- Name: puzzles; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.puzzles (
    id integer NOT NULL,
    name character varying(64) NOT NULL,
    kind_id integer NOT NULL,
    scramble_length integer,
    attempt_count integer DEFAULT 1 NOT NULL,
    countdown integer DEFAULT 15 NOT NULL,
    average_format character varying(255) DEFAULT 'average'::character varying NOT NULL,
    version integer DEFAULT 0,
    css_position integer DEFAULT 0 NOT NULL,
    slug character varying(255),
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL
);


ALTER TABLE public.puzzles OWNER TO postgres;

--
-- Name: puzzles_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.puzzles_id_seq
    START WITH 32
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.puzzles_id_seq OWNER TO postgres;

--
-- Name: puzzles_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.puzzles_id_seq OWNED BY public.puzzles.id;


--
-- Name: records; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.records (
    id integer NOT NULL,
    "time" integer NOT NULL,
    puzzle_id integer NOT NULL,
    user_id integer NOT NULL,
    amount integer NOT NULL,
    set_at timestamp without time zone NOT NULL,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL,
    comment character varying(255)
);


ALTER TABLE public.records OWNER TO postgres;

--
-- Name: records_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.records_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.records_id_seq OWNER TO postgres;

--
-- Name: records_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.records_id_seq OWNED BY public.records.id;


--
-- Name: records_singles; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.records_singles (
    record_id integer NOT NULL,
    single_id integer NOT NULL
);


ALTER TABLE public.records_singles OWNER TO postgres;

--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.schema_migrations (
    version character varying(255) NOT NULL
);


ALTER TABLE public.schema_migrations OWNER TO postgres;

--
-- Name: sessions; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.sessions (
    id integer NOT NULL,
    session_id character varying(255) NOT NULL,
    data text,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL
);


ALTER TABLE public.sessions OWNER TO postgres;

--
-- Name: sessions_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.sessions_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.sessions_id_seq OWNER TO postgres;

--
-- Name: sessions_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.sessions_id_seq OWNED BY public.sessions.id;


--
-- Name: users; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.users (
    id integer NOT NULL,
    name character varying(32) NOT NULL,
    email character varying(255) NOT NULL,
    salt character varying(8) NOT NULL,
    encrypted_password character varying(255) NOT NULL,
    created_at timestamp without time zone NOT NULL,
    role character varying(16) DEFAULT 'user'::character varying,
    wca character varying(255),
    time_zone character varying(100) DEFAULT 'UTC'::character varying NOT NULL,
    ignored boolean DEFAULT false NOT NULL,
    updated_at timestamp without time zone NOT NULL,
    wants_emails boolean DEFAULT false NOT NULL,
    singles_count integer DEFAULT 0 NOT NULL,
    slug character varying(255),
    wasted_time bigint DEFAULT 0 NOT NULL
);


ALTER TABLE public.users OWNER TO postgres;

--
-- Name: users_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.users_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.users_id_seq OWNER TO postgres;

--
-- Name: users_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.users_id_seq OWNED BY public.users.id;


--
-- Name: activities id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.activities ALTER COLUMN id SET DEFAULT nextval('public.activities_id_seq'::regclass);


--
-- Name: comments id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.comments ALTER COLUMN id SET DEFAULT nextval('public.comments_id_seq'::regclass);


--
-- Name: cubing_sessions id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cubing_sessions ALTER COLUMN id SET DEFAULT nextval('public.cubing_sessions_id_seq'::regclass);


--
-- Name: delayed_jobs id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.delayed_jobs ALTER COLUMN id SET DEFAULT nextval('public.delayed_jobs_id_seq'::regclass);


--
-- Name: followings id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.followings ALTER COLUMN id SET DEFAULT nextval('public.followings_id_seq'::regclass);


--
-- Name: kinds id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.kinds ALTER COLUMN id SET DEFAULT nextval('public.kinds_id_seq'::regclass);


--
-- Name: posts id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.posts ALTER COLUMN id SET DEFAULT nextval('public.posts_id_seq'::regclass);


--
-- Name: puzzles id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.puzzles ALTER COLUMN id SET DEFAULT nextval('public.puzzles_id_seq'::regclass);


--
-- Name: records id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.records ALTER COLUMN id SET DEFAULT nextval('public.records_id_seq'::regclass);


--
-- Name: sessions id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.sessions ALTER COLUMN id SET DEFAULT nextval('public.sessions_id_seq'::regclass);


--
-- Name: singles id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.singles ALTER COLUMN id SET DEFAULT nextval('public.clocks_id_seq'::regclass);


--
-- Name: users id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.users ALTER COLUMN id SET DEFAULT nextval('public.users_id_seq'::regclass);


--
-- Name: activities activities_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.activities
    ADD CONSTRAINT activities_pkey PRIMARY KEY (id);


--
-- Name: singles clocks_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.singles
    ADD CONSTRAINT clocks_pkey PRIMARY KEY (id);


--
-- Name: comments comments_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.comments
    ADD CONSTRAINT comments_pkey PRIMARY KEY (id);


--
-- Name: cubing_sessions cubing_sessions_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cubing_sessions
    ADD CONSTRAINT cubing_sessions_pkey PRIMARY KEY (id);


--
-- Name: delayed_jobs delayed_jobs_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.delayed_jobs
    ADD CONSTRAINT delayed_jobs_pkey PRIMARY KEY (id);


--
-- Name: followings followings_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.followings
    ADD CONSTRAINT followings_pkey PRIMARY KEY (id);


--
-- Name: kinds kinds_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.kinds
    ADD CONSTRAINT kinds_pkey PRIMARY KEY (id);


--
-- Name: posts posts_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.posts
    ADD CONSTRAINT posts_pkey PRIMARY KEY (id);


--
-- Name: puzzles puzzles_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.puzzles
    ADD CONSTRAINT puzzles_pkey PRIMARY KEY (id);


--
-- Name: records records_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.records
    ADD CONSTRAINT records_pkey PRIMARY KEY (id);


--
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: sessions sessions_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.sessions
    ADD CONSTRAINT sessions_pkey PRIMARY KEY (id);


--
-- Name: users users_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);


--
-- Name: delayed_jobs_priority; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX delayed_jobs_priority ON public.delayed_jobs USING btree (priority, run_at);


--
-- Name: index_followings_on_followee_id; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX index_followings_on_followee_id ON public.followings USING btree (followee_id);


--
-- Name: index_followings_on_follower_id; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX index_followings_on_follower_id ON public.followings USING btree (follower_id);


--
-- Name: index_posts_on_created_at; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX index_posts_on_created_at ON public.posts USING btree (created_at);


--
-- Name: index_puzzles_on_slug; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX index_puzzles_on_slug ON public.puzzles USING btree (slug);


--
-- Name: index_records_on_puzzle_id_and_amount_and_time; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX index_records_on_puzzle_id_and_amount_and_time ON public.records USING btree (puzzle_id, amount, "time");


--
-- Name: index_records_singles_on_record_id_and_single_id; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX index_records_singles_on_record_id_and_single_id ON public.records_singles USING btree (record_id, single_id);


--
-- Name: index_sessions_on_session_id; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX index_sessions_on_session_id ON public.sessions USING btree (session_id);


--
-- Name: index_sessions_on_updated_at; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX index_sessions_on_updated_at ON public.sessions USING btree (updated_at);


--
-- Name: index_singles_on_user_id_and_puzzle_id_and_created_at; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX index_singles_on_user_id_and_puzzle_id_and_created_at ON public.singles USING btree (user_id, puzzle_id, created_at);


--
-- Name: index_unique_records_on_puzzle_id_amount_user_id; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX index_unique_records_on_puzzle_id_amount_user_id ON public.records USING btree (puzzle_id, amount, user_id);


--
-- Name: index_users_on_email; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX index_users_on_email ON public.users USING btree (email);


--
-- Name: index_users_on_name_and_encrypted_password; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX index_users_on_name_and_encrypted_password ON public.users USING btree (name, encrypted_password);


--
-- Name: index_users_on_singles_count; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX index_users_on_singles_count ON public.users USING btree (singles_count);


--
-- Name: index_users_on_slug; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX index_users_on_slug ON public.users USING btree (slug);


--
-- Name: unique_schema_migrations; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX unique_schema_migrations ON public.schema_migrations USING btree (version);


--
-- PostgreSQL database dump complete
--

