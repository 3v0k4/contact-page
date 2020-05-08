---
title: Rewriting to Haskell–Configuration
description: Using yaml to configure a Servant application
author: Riccardo
tags:
  - Functional Programming
  - Haskell
  - Servant
---

This is part of a series:

- [Rewriting to Haskell–Intro](https://odone.io/posts/2020-02-26-rewriting-haskell-intro.html)
- [Rewriting to Haskell–Project Setup](https://odone.io/posts/2020-03-03-rewriting-haskell-setup.html)
- [Rewriting to Haskell–Deployment](https://odone.io/posts/2020-03-14-rewriting-haskell-server.html)
- [Rewriting to Haskell–Automatic Formatting](https://odone.io/posts/2020-03-19-rewriting-haskell-formatting.html)

---

Coming from Rails we are used to employing yaml files to configure a web application. This is why we decided to do the same with Servant. As a matter of fact, we now have a `configuration.yml` file:

```yml
database:
  username: stream
  database: stream_development
  password: ""

application:
  aws_s3_access_key: "ABCD1234"
  aws_s3_secret_key: "EFGH5678"
  aws_s3_region: us-east-1
  aws_s3_bucket_name: stream-demo-bucket
```

That is great for development but how can we run test against the test database? Turns out that the package we use to parse the yaml file allows the use of ENV variables:

```yml
database:
  username: stream
  database: _env:DATABASE:stream_development
  password: ""

application:
  aws_s3_access_key: "ABCD1234"
  aws_s3_secret_key: "EFGH5678"
  aws_s3_region: us-east-1
  aws_s3_bucket_name: stream-demo-bucket
```

That is, now we can just run `DATABASE=stream_test stack test`!

In the repository we actually keep a `configuration.yml.example` file and git ignore `configuration.yml` to avoid leaking credentials:

```yml
database:
  username: stream
  database: _env:DATABASE:stream_development
  password: ""

application:
  aws_s3_access_key: "REPLACE_ME"
  aws_s3_secret_key: "REPLACE_ME"
  aws_s3_region: us-east-1
  aws_s3_bucket_name: ll-stream-demo

```

For production we use [Ansible](https://www.ansible.com/) (with Ansible Vault) to put in place the correct `configuration.yml`. Plus, we instruct [Hapistrano](https://hackage.haskell.org/package/hapistrano) to make that file available for each deployment:

```yml
linked_files:
  - haskell/configuration.yml
```

To read the configuration inside the Servant application we use [`loadYamlSettings`](https://www.stackage.org/haddock/lts-15.5/yaml-0.11.3.0/Data-Yaml-Config.html#v:loadYamlSettings) from the [yaml](https://www.stackage.org/package/yaml) package:

```hs
loadYamlSettings
    :: FromJSON settings
    => [FilePath] -- ^ run time config files to use, earlier files have precedence
    -> [Value] -- ^ any other values to use, usually from compile time config. overridden by files
    -> EnvUsage
    -> IO settings
```

In other words, given a type `settings` that is an instance of `FromJSON` we can decode yaml files into a value of that type. And this is how we do it for Stream:

```hs
data Configuration
  = Configuration
      { configurationDatabaseUser :: String,
        configurationDatabaseDatabase :: String,
        configurationDatabasePassword :: String,
        configurationApplicationAwsS3AccessKey :: AccessKey,
        configurationApplicationAwsS3SecretKey :: SecretKey,
        configurationApplicationAwsS3Region :: Region,
        configurationApplicationAwsS3BucketName :: BucketName
      }

instance FromJSON Configuration where
  parseJSON (Object x) = do
    database <- x .: "database"
    application <- x .: "application"
    Configuration
      <$> database .: "username"
      <*> database .: "database"
      <*> database .: "password"
      <*> application .: "aws_s3_access_key"
      <*> application .: "aws_s3_secret_key"
      <*> application .: "aws_s3_region"
      <*> application .: "aws_s3_bucket_name"

loadConfiguration :: IO Configuration
loadConfiguration =
  loadYamlSettings ["./configuration.yml"] [] useEnv
```
