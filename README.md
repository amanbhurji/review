# Review

Run the server with `cabal new-run review`.
Create a new paste at http://localhost:8081/pages/newpaste.html

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

You can now connect to it via various postgres libraries.
For example -
You should now be able to connect by providing a password to the
`defaultConnectInfo` from `Database.PostgreSQL.Simple` in `postgresql-simple`

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

