# Makefile ---------------------------------------------------------------------
default_target: help

.PHONY: help
help: ## show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'


SERVICE = "airflow-scheduler"

# Env ---------------------------------------------------------------------
.PHONY: init
init:
	./scripts/init.sh
	docker-compose run airflow-init
	# docker-compose run \
		# -e "_AIRFLOW_DB_UPGRADE=true" \
		# -e "_AIRFLOW_WWW_USER_CREATE=true" \
		# -e "_AIRFLOW_WWW_USER_USERNAME=airflow" \
		# -e "_AIRFLOW_WWW_USER_PASSWORD=airflow" \
	# --rm $(SERVICE) db init


# Build ---------------------------------------------------------------------
.PHONY: build
build:
	docker-compose build $(SERVICE)
