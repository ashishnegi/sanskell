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

start-server: server-setup server-build
	strip /usr/.local/bin/sanskell-exe
	AppEnv=prod ROOT_URL=https://sanskell.herokuapp.com /usr/.local/bin/sanskell-exe
