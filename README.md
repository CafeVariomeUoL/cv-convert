# cv-convert
![CI](https://github.com/CafeVariomeUoL/cv-convert/workflows/CI/badge.svg)

Document converter used inside [CafeVariomeCI4](https://github.com/CafeVariomeUoL/CafeVariomeCI4). 


## Usage

The tool has a command line interface with the following basic usage:

```
cv-convert (-i|--input STRING) (-s|--settings STRING) 
           [-o|--output STRING] [--env STRING] [--source-id INT] 
           [--db-config STRING] [--log STRING] [--terminate-on-error] 
           [--write-record-count] [-v|--verbose]
```

The required arguments are `-i` for passing in the path to the input file and `-s` for passing in the path to
the `.settings` file, used to convert the file. When no other flags are passed as arguments, the output is written
into a JSON file at `<path_to_input_file>.out.json` and log outputs go to `<path_to_input_file>.log`.

For different logging behaviour, set the `--log` flag to one of `console`, `file` or `db`. The default behaviour of the program is to skip any line where an error occurs and carry on with the rest of the file. If this is not the desired behaviour, pass the `--terminate-on-error` flag to the CLI.

The flag `-o` takes the values:

- `json` - directing the output into a JSON file.
- `sql`- directing the output into an SQL statement file. The records are serialised into EAV triples and the insert statements produced are of the form:
    ```sql
    INSERT INTO eavs(uid, source, fileName, subject_id, type, attribute, value, elastic) 
    VALUES ('de8a9663-6434-4050-bdf8-8d47552b7542', '1', 0, '0', 'attribute', 'subject_id', '0', 0);
    ```
    (See the section `DB` below for more info)
- `db` - directing output into a dababase. Note: This flag must be used together with either:
    *   the `--env` flag, used to specify the path to an ENV file, which must contain the `host`, `dbname`, `user`, `password` and `db` (with value `mysql` or `postgres`), or
    *   the `--db-config` flag, passing in a DB connection URI in the form `db_type://user:password@host:port/dbname`
    
    These parameters are used to establish a connection to the DB. When writing to a `postgres` backend, the output is stored as
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

    The records are also serialised into EAV triples using [`flattenToEAV`](https://cafevariomeuol.github.io/cv-convert/JSON-Utils.html#v:flattenToEAV) and stored in the `eavs` table (both when `db` is `postgres` or `mysql`):

    ```sql
    CREATE TABLE eavs (
        id serial primary key,
        uid character varying(50) NOT NULL,
        source_id integer NOT NULL,
        "fileName" integer NOT NULL,
        subject_id text NOT NULL,
        type character varying(20) NOT NULL,
        attribute text NOT NULL,
        value text,
        elastic boolean DEFAULT false NOT NULL
    );
    ```

The `--write-record-count` flag updates the `record_count` column in `sources`, when writing to the database.

---
**NOTE**

When using the `-o sql` or `-o db` options, the `source_id` argument must also be provided via `--source-id <source_id>`, as it is needed when inserting into the tables above.

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

For tests, run `./stack-test-with-db`. The script uses docker to create a temporary Postgres and MySQL DBs to run the test suite. 
If you only want to run tests not involving the DB, you can do so via running `stack test`.

The tests are located in the `test/` folder and mirror the structure of the code inside the `src` folder, i.e.
for `src/JSON/Utils.hs` we will have a corresponding test file `test/JSON/Utils/Tests.hs`. All tests are collected and launched via `test/Spec.hs`.


---
**NOTE**

If running Stack locally on a mac, I would recommend also running `./stack-test-with-db -d`, which runs the tests inside a docker container. This option is provided, as there might be subtle differences between macOS and Linux.

---

### Azure pipelines

The test suite is compiled and run multi-threaded. However, [QuickJS](https://bellard.org/quickjs/) (the JS engine we use for processing records inside the tool) does not like 
being run in a multi-threaded setting. Due to the way Haskell threads are mapped to OS threads, a Haskell thread can be run on different OS threads throughout its lifetime. 
However, this is problematic for QuickJS, which seems to be tied to the OS thread it was started on.

A fix for running locally involves running tests inside [`quickjsMultithreaded`](https://hackage.haskell.org/package/quickjs-hs-0.1.2.1/docs/Quickjs.html#t:quickjsMultithreaded) rather than the 
[`quickjs`](https://hackage.haskell.org/package/quickjs-hs-0.1.2.1/docs/Quickjs.html#t:quickjs) environment). 
The difference between those two involves running all the QuickJS IO actions wrapped in a [`runInBoundThread`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Concurrent.html#v:runInBoundThread), 
which binds the Haskell thread to the OS thread it was created in. This works on my local machine, but fails on Azure pipleines with:

```bash
lost signal due to full pipe: 11
forkOS_entry: interrupted
```

The only fix I've managed to get to working is to link the test executable as non-threaded and make all the tests run sequentially... 
This should not be an issue unless we have a **LOT** of tests. The fix involves passing `--flag 'cv-convert:azure'` to Stack, when compiling and running the tests on Azure.

## Profiling

To get a heap profile from a run, compile the project using `stack build --profile`, then run via the `stack-profile` script, e.g.:

```
./stack-profile -i input.vcf -s vcf.settings -o sql --source-id 1 --log console
```

A new file called `cv-convert.ps` will be created, showing the heap usage for the given execution.

## Releases

At the moment, we upload compiled binaries to github manually. Might change this in the future if it's worth the reduced hassle. To build and copy the binary to an output folder locally, run

```bash
stack install --local-bin-path=<path_to_output_folder>
```

## TODO

Set up automatic binary builds for tagged releases?
