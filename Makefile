all: setup build

setup: ui-setup server-setup

build: ui-build server-build

ui-setup:
	(cd ui ; elm package install -y)

ui-build:
	(cd ui ; make)

server-setup:
	stack setup

server-build:
	stack build
