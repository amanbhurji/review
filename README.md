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

### Sample json output
```json
https://hastebin.com/raw/cifujujoqu

IT WONT LET ME PASTE PREFORMATTED JSON IN HERE!
```
