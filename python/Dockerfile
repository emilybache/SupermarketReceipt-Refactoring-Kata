FROM alpine AS base

RUN apk add python3 py3-pip
RUN mkdir /src
WORKDIR /src
COPY requirements.txt /src
RUN pip install -r requirements.txt

FROM base AS test

RUN pip install coverage mypy
COPY . /src
CMD mypy . && \
    coverage run --source supermarket/ -m pytest . && \
    coverage report -m && \
    coverage html -d /output/htmlcov
