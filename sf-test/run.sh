#!/bin/sh
erl -pa deps/amqp_client/ebin -pa deps/bunny_farm/ebin -pa deps/bson/ebin -pa deps/lager/ebin/ -pa deps/rabbit_common/ebin -pa ebin
