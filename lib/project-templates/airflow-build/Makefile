# python version
PYTHON_BASE_IMAGE=python:3.8-slim-buster
PYTHON_MAJOR_VERSION=3.8
# airflow extra providers
AIRFLOW_EXTRAS=celery,amazon,slack,sftp
# image build extra packages
ADDITIONAL_DEV_APT_DEPS=llvm gcc
# image runtime extra packages
ADDITIONAL_RUNTIME_APT_DEPS=git llvm build-essential
# image tag
TAG=airflow-build-python38

airflow:
	git clone https://github.com/apache/airflow.git airflow


.PHONY: build
build: | airflow
	# See https://github.com/apache/airflow/blob/master/Dockerfile for all ARGS
	docker build airflow -f airflow/Dockerfile \
		--build-arg PYTHON_BASE_IMAGE=$(PYTHON_BASE_IMAGE) \
		--build-arg PYTHON_MAJOR_MINOR_VERSION=$(PYTHON_MAJOR_MINOR_VERSION) \
		--build-arg AIRFLOW_EXTRAS="$(AIRFLOW_EXTRAS)" \
		--build-arg ADDITIONAL_DEV_APT_DEPS="$(ADDITIONAL_DEV_APT_DEPS)" \
		--build-arg ADDITIONAL_RUNTIME_APT_DEPS="$(ADDITIONAL_RUNTIME_APT_DEPS)" \
		--tag $(TAG)

