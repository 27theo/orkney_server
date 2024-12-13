(lang dune 3.18)

(name server)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Theo Addis <theocaddis@gmail.com>")

(maintainers "Theo Addis <theocaddis@gmail.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name server)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
   ocaml
   dream
   dotenv
   caqti-driver-sqlite3)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
