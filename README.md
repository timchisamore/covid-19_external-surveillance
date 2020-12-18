This R project takes Case and Contact Managment (CCM), Acute Care Enhanced Surveillance (ACES), and Ministry of Health and Long-Term Care (MOHLTC) daily bed census (DBC) data and produces an HTML COVID-19 surveillance report using RMarkdown.

The intentions of the project are to automate the process and to make dissemination of the product easier. We used the drake package to manage our workflow as there is quite a bit going on in terms of data cleaning and analysis with multiple datasources. we also use the renv package to capture the packages versions being used to ensure long-term stability of the report.

Note, this project originally used integrated Public Health Information System (iPHIS) data, but this was migrated into CCM on 2020/08/13 and CCM will be used for all COVID-19 case and contact managment going forward. The old functions, output, documents, and data from the iPHIS version of the report can be found in the archive folder.

After updating the data, the document can be created by typing drake::r_make() into the console.