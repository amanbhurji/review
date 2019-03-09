# Review

## API

```
POST  /paste              create a new paste

GET   /paste/:id          get a paste

POST  /paste/:id/comment  create a comment
```

Sample commands to test the api

```
curl http://localhost:8081/pastes

curl -X POST -d '"This is a posted paste!\nNewline?\nAnother?"' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/paste 

curl http://localhost:8081/paste/<pId>

curl -X POST -d '"This is a top level comment!"' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/paste/<pId>/comment
```


-----
f :: Show a => a -> IO String
f a = do
  print "In f:"
  print a
  pure $ show a

g = putStrLn . ("In g:" <>)

h = length