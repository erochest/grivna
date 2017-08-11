
APP=grivna
REPO=erochest/grivna-grivna

build:
	stack build --pedantic

test: build
	stack test

deploy: build test
	stack --docker-run-args='--net=bridge --publish=3000:3000' image container
	docker push $(REPO)
	cf push $(APP) --docker-image $(REPO)

