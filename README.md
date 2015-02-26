# What?

Simple types and set of functions to quickly add configuration of
postgresql to your Yesod site or whatever.

# How?

Add separate file or section inside your existing config like that

```yml
database:    "dbname"
host:        "127.0.0.1"        # optional
port:        "5432"             # optional
user:        "dbuser"
password:    "pass"
poolsize:    "10"               # optional maximum connections in pool
pooltimeout: "60"               # optional minimum connection lifetime
poolstripes: "1"                # optional count of stripes in pool
```

and then in your program something like that

```haskell

pool <- decodeFile "pgconfig.yml"
         >>= maybe (fail "Could not parse pgconfig.yml")
             createPGPool
pingPGPool pool
```

So now you have a pool and can perform queries any way you like.