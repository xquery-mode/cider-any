# Development environment

This instructions will help you to build development quickly.

* Download MackLogic server in to current directory

```shell
curl <mark logic url> -O MarkLogic-RHEL7-8.0-5.8.x86_64.rpm
```

* Build docker image

```shell
docker build -t marklogic:8 .
```

* Start MarkLogic server inside docker

```shell
docker run -t -i --name cider-any -p 7997:7997 -p 7999:7999 -p 8000:8000 -p 8001:8001 -p 8002:8002 -p 8889:8889 marklogic:8
```

* You can pause MarkLogic server

```shell
docker stop cider-any
```

* You can resume MarkLogic server

```shell
docker start -i -a cider-any
```

* Open http://localhost:8001/ in your browser and follow installation steps

** Forests / Create
   - CiderAnyForest

** Databases / Create
   - TutorialDB

** Databases / TutorialDB / Forests
   - Attach it to the `CiderAnyForest`

** Groups / Default / App Servers / Create XDBC
   - CiderAny
   - /data
   - 8889

* Run Emacs

```shell
cask exec emacs -l scripts/init.el -f cider-jack-in
```
