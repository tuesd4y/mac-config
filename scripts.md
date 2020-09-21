# Helpful scripts

## Kill app at port

```bash
lsof -t -i :4201 | xargs kill
```

## Set localtunnel as SERVER URL environment

```bash
tunnel 8080
```

- set up localtunnel for port 8080
- add export line to `~/config/private/export.sh`
- this saves the URL to the `SERVER_URL` environment variable when a new shell is created

## Helpful tools

- localtunnel - `npm i -g localtunnel` ->  `lt -p 8080`
- Serve - `npm i -g serve` -> `serve .`
- Copy all files from bills folder for a given month

```bash
 cp 2020_08_*/*.* ~/docs/print
```

- move last n commits into new branch and reset old to before
  (source: [howchoo.com](https://howchoo.com/git/git-move-your-latest-commits-to-another-branch)

```bash
git branch <new branch>
git reset --keep HEAD~<N>
git checkout <new branch>
```
