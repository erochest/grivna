
APP=grivna
REPO=erochest/grivna-grivna

run:
	stack build --pedantic --exec 'stack exec -- grivna'

build:
	stack build --pedantic

watch:
	stack build --file-watch

test:
	stack build --test

clean:
	stack clean

deploy: test
	stack --docker clean
	stack --docker build --test
	stack --docker --docker-run-args='--net=bridge --publish=8080:8080' \
		image container
	docker push $(REPO)
	cf push $(APP) --docker-image $(REPO)

