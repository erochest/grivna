
APP=grivna
REPO=erochest/grivna-grivna

run:
	LOG_LEVEL=DEBUG stack build --pedantic --exec 'stack exec -- grivna'

build:
	stack build $(BUILD_OPTS)

watch:
	stack build --file-watch $(BUILD_OPTS)

test:
	stack build --test $(BUILD_OPTS)

clean:
	stack clean

deploy: test
	stack --docker clean
	stack --docker build --test --pedantic
	stack --docker --docker-run-args='--net=bridge --publish=8080:8080' \
		image container
	docker push $(REPO)
	cf push $(APP) --docker-image $(REPO)

