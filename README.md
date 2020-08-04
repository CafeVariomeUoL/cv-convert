# cv-convert
[![Build Status](https://dev.azure.com/sb911/cv-convert-haskell/_apis/build/status/CafeVariomeUoL.cv-convert?branchName=master)](https://dev.azure.com/sb911/cv-convert-haskell/_build/latest?definitionId=1&branchName=master)

Document converter used inside [CafeVariomeCI4](https://github.com/CafeVariomeUoL/CafeVariomeCI4). 


## Usage

The tool has a command line interface with the following basic usage:

```
cv-convert (-i|--input STRING) (-s|--settings STRING) [-d|--db] 
           [-e|--env STRING] [-s|--source_id SOURCEID]
```

The required arguments are `-i` for passing in the path to the input file and `-s` for passing in the path to
the `.settings` file, used to convert the file. When no other flags are passed as arguments, the output is written
into a JSON file at `<path_to_input_file>.out.json`

The flag `-d` indicates that the output should written directly into the database. This flag must be used together
with the `-e` flag, used to specify the path to an ENV file, which must contain the `host`, `dbname`, `user` and `password`
variables. These are used to establish a connection to the Postgres DB. When in DB mode, the output is stored as
JSONB data in the `eavs_jsonb` table with the following schema:

```sql
CREATE TABLE eavs_jsonb (
    id serial primary key,
    source_id integer NOT NULL,
    "fileName" integer NOT NULL,
    subject_id text NOT NULL,
    data jsonb NOT NULL,
    unique(source_id,subject_id),
    CONSTRAINT eavs_jsonb_source_id_fkey FOREIGN KEY (source_id)
        REFERENCES sources (source_id) MATCH SIMPLE
        ON UPDATE CASCADE
        ON DELETE NO ACTION
);
```

Metadata about the objects, stored in `eavs_jsonb` is then written into the `eavs_jsonb_attributes_values` table:

```sql
CREATE TABLE eavs_jsonb_attributes_values (
    id serial primary key,
    source_id integer NOT NULL,
    attribute jsonb NOT NULL,
    "values" jsonb NOT NULL,
    unique(source_id,attribute),
    CONSTRAINT eavs_jsonb_attributes_source_id_fkey FOREIGN KEY (source_id)
        REFERENCES sources (source_id) MATCH SIMPLE
        ON UPDATE CASCADE
        ON DELETE CASCADE
);
```

For more details on precisely what data gets stored in this table, see the [`createAllPathsWithValues`](https://cafevariomeuol.github.io/cv-convert/JSON-Utils.html#v:createAllPathsWithValues)
function.

The records are also serialised into EAV triples using [`flattenToEAV`](https://cafevariomeuol.github.io/cv-convert/JSON-Utils.html#v:flattenToEAV) and stored in the `eavs` table:

```sql
CREATE TABLE eavs (
    id serial primary key,
    uid character varying(50) NOT NULL,
    source_id character varying(50) NOT NULL,
    "fileName" integer NOT NULL,
    subject_id text NOT NULL,
    type character varying(20) NOT NULL,
    attribute text NOT NULL,
    value text,
    elastic boolean DEFAULT false NOT NULL
);
```

---
**NOTE**

When using the `-d` flag, the `source_id` must also be provided via `-s <source_id>`, as its needed when inserting into the tables above.

---

## Documentation
Haddocs documentation is available at: https://cafevariomeuol.github.io/cv-convert/

To rebuild the docs, commit all changes to your current branch and run `./stack-gh-pages` 


## Compiling

This tool is written in Haskell and uses the [Stack](https://docs.haskellstack.org/en/stable/README/) tool to manage the compilation and testing. 
Follow the instructions [here](https://docs.haskellstack.org/en/stable/install_and_upgrade/) to set up Stack on your local machine.

Then, you can simply run `stack build`. However, to ensure (hopefully) consistent builds and avoid dealing with external dependencies,
we can use docker to build the project. This is also the way to build linux binaries on mac. 
First, we need to build the docker image which will be used by Stack to compile the code:

```bash
docker build --tag static-haskell-alpine-8.8.3 .
```

Afterwards, we can run

```bash
stack build --docker --docker-image static-haskell-alpine-8.8.3
```

## Tests

For tests, run `./stack-test-with-db`. The script uses docker to create a temporary Postgres DB to run the test suite, including DB tests. 
If you only want to run tests not involving the DB, you can do so via `stack test`.

The tests are located in the `test/` folder and mirror the structure of the code inside the `src` folder, i.e.
for `src/JSON/Utils.hs` we will have a corresponding test file `test/JSON/Utils/Tests.hs`. All tests are collected and launched via `test/Spec.hs`.


---
**NOTE**

If running Stack locally on a mac, I would recommend also running `./stack-test-docker-with-db`, as there might be subtle differences that might show on Linux.

---

### Azure pipelines

The test suite is compiled and run multi-threaded. However, [QuickJS](https://bellard.org/quickjs/) (the JS engine we use for processing records inside the tool) does not like 
being run in a multi-threaded setting. Due to the way Haskell threads are mapped to OS threads, a Haskell thread can be run on different OS threads throughout its lifetime. 
However, this is problematic for QuickJS, which seems to be tied to the OS thread it was started on.

A fix for running locally involves running tests inside [`quickjsTest`](https://cafevariomeuol.github.io/cv-convert/Quickjs.html#v:quickjsTest) rather than the 
[`quickjs`](https://cafevariomeuol.github.io/cv-convert/Quickjs.html#v:quickjs) environment). 
The difference between those two involves running all the QuickJS IO actions wrapped in a [`runInBoundThread`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Concurrent.html#v:runInBoundThread), 
which binds the Haskell thread to the OS thread it was created in. This works on my local machine, but fails on Azure pipleines with:

```bash
lost signal due to full pipe: 11
forkOS_entry: interrupted
```

The only fix I've managed to get to working is to link the test executable as non-threaded and make all the tests run sequentially... 
This should not be an issue unless we have a **LOT** of tests. The fix involves passing `--flag 'cv-convert:azure'` to Stack, when compiling and running the tests on Azure.

## TODO

Set up automatic binary builds for tagged releases?