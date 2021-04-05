FROM apache/airflow:2.0.1

# downgrade azure connectors, stock version has breaking changes
RUN pip install apache-airflow-providers-microsoft-azure==1.2.0rc1
