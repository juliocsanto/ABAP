# ABAP Parallel Processing

The idea of this simple code is to get all materials of a branch (from MARC table) and obtain MRP information in order to show only those which availability is negative.
To do so is used parallel processing using the *CALL FUNCTION STARTING NEW TASK DESTINATION IN GROUP* RFC approach. 

By doing that, the program that used to execute about 15 minutes executed up to 1 minute.

Sources to review and study to achieve even better performance in ABAP code:
- Youtube Video: https://www.youtube.com/watch?v=rq4v22HgtOs 
- SAP Documentation: https://help.sap.com/doc/saphelp_nw73ehp1/7.31.19/en-us/4d/909309eba36e73e10000000a15822b/content.htm?no_cache=true#:~:text=Parallel%20processing%20is%20implemented%20in,if%20they%20are%20started%20interactively 
- SAP Blog: https://blogs.sap.com/2014/04/28/two-different-types-of-parallel-processing-examples/ 
- SAP Blog: https://blogs.sap.com/2021/08/26/async-parallel-abap-in-a-oo-way/ 
- SAP Blog: https://blogs.sap.com/2019/03/19/parallel-processing-made-easy/
