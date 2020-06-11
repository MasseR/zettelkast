# Zettelkast

Command-line tool for managing zettelkast documents. The tool primarily focuses
on providing unique ids and showing a graph of document connections. It tries
to be as unintrusive as possible.

The only requirement is setting the `ZETTELKAST_ROOT` environment variable to
where you want to store your zettels.

## What is a zettelkast zettel

A zettelkast zettel is just a normal markdown file with a unique zettelkast
style filename. Aside from the filename, you can write your normal everyday
markdown with no special consideration of any kind.

- The first layer of header1 is used as the title when listing with `zettelkast list`
- Any links across zettels are just normal links to files. No special syntax or
  identifiers needed.

## Creating a new zettel

```
$ zettelkast new
```

It will open your `$EDITOR` with the filename, defaulting to `vim`.

## Showing the document graph

```
$ zettelkast graph | sfdp -x -Goverlap=scale -Txlib
```

The `zettelkast graph` command will output the connections graph as a dot
output, you can pipe it to graphviz to render it.
