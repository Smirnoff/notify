FROM ocaml/opam

USER root
RUN apt-get -y update && apt-get -y install libssl-dev

# installing os
USER opam
RUN eval `opam config env` && \
  opam install -y lwt ssl cohttp
RUN eval `opam config env` && \
  opam install -y core
RUN eval `opam config env` && \
  opam install -y config-file cryptohash yojson websocket

# grabbing source
USER root

COPY . /home/opam/src
RUN chown -R opam:opam /home/opam/src

USER opam
WORKDIR /home/opam/src
RUN ./clean.sh
RUN eval `opam config env` && ocamlbuild -use-ocamlfind run_all.native

CMD ./run_all.native
