# Review

## API

```
POST  /paste              create a new paste

GET   /paste/:id          get a paste

POST  /paste/:id/comment  create a comment
```

### Sample curl commands to test the api

```
curl http://localhost:8081/pastes

curl -X POST -d '"This is a posted paste!\nNewline?\nAnother?"' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/paste 

curl http://localhost:8081/paste/<pId>

curl -X POST -d '"This is a top level comment!"' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/paste/<pId>/comment

curl -X POST -d '"This is a line comment!"' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/paste/<pId>/comment?line=3

```

### Sample response to `/paste/:id`
```json
{
  "id": "f2dee7b5-624f-4248-a190-a1584daf9a5c",
  "lines": [
    {
      "line": "This is a posted paste!",
      "comments": []
    },
    {
      "line": "Newline?",
      "comments": [
        {
          "body": "This is a line comment!"
        }
      ]
    },
    {
      "line": "Another?",
      "comments": [
        {
          "body": "This is yet another line comment!"
        },
        {
          "body": "This is another line comment!"
        }
      ]
    }
  ],
  "comments": [
    {
      "body": "This is another toplevel comment!"
    },
    {
      "body": "This is a toplevel comment!"
    }
  ]
}
```

## Database setup

Pre-requirements -
  - docker
  - libpqxx-devel (for pg_config, postgresql-devel might be sufficient)

### Create a docker postgres container
```
docker run --name try-postgres-1 -d -p 5432:5432 \
-v /home/sour/tmp/data:/var/lib/postgresql/data -e POSTGRES_PASSWORD=password \
postgres

```

You should now be able to connect by providing a password to the
`defaultConnectInfo` from `Database.PostgreSQL.Simple` in `postgresql-simple`


# Schema
Paste is a collection of lines
Line is text + any comments
Comment is text

--------

Paste+Line
pid
lno
text

Comment
pid
lno
cno
text

CREATE TABLE pastes (
  pid UUID,
  line_number SMALLINT NOT NULL,
  line_text TEXT,
  PRIMARY KEY (pid, line_number)
);

CREATE TABLE comments (
  body TEXT NOT NULL,
  comment_number SMALLINT NOT NULL,
  line_number SMALLINT,
  pid UUID,
  PRIMARY KEY (pid, line_number, comment_number),
  FOREIGN KEY (pid, line_number) REFERENCES pastes
);

---------

Paste
pid
text

Comment
pid
lno
cno
text

CREATE TABLE pastes (
  pid UUID PRIMARY KEY,
  body Text NOT NULL
);

CREATE TABLE comments (
  line_number SMALLINT NOT NULL,
  comment_number SMALLINT NOT NULL,
  body TEXT NOT NULL,
  pid UUID REFERENCES pastes(pid) ON DELETE CASCADE,
  PRIMARY KEY (pid, line_number, comment_number)
);
--------

