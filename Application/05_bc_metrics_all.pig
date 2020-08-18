register 'bc_udf.py' using jython as bcudf;

cdr = LOAD '/D4D/CDR/2013/XX'  -- load all data from month XX, perform this script for each month XX = (01, ..., 12)
using PigStorage(';') as (
call_record_type:chararray,
caller_msisdn: chararray,
call_date:chararray,
basic_service:chararray,
ms_location:chararray,
call_partner_identity_type:int,
call_partner_identity:chararray,
tac_code:chararray,
call_duration:int);

cells = LOAD '/D4D/LOC/ms_location' using PigStorage(';') as (
ms_location: chararray,
site_id: int,
region_id: int,
x: int,
y: int,
lon:float,
lat:float);

filtered_cdr = FILTER cdr BY call_date IS NOT NULL AND call_date!='' AND call_date!=' ' AND ms_location!=' ' AND ms_location!=''
AND (call_record_type=='1' OR call_record_type=='2')
AND (basic_service=='11' OR basic_service=='12' OR basic_service=='21' OR basic_service=='22');

cdr_cells = JOIN filtered_cdr BY ms_location, cells BY ms_location;

cdr_cells_for_bc = FOREACH cdr_cells GENERATE call_record_type,basic_service,caller_msisdn,call_partner_identity,call_date,call_duration,lon,lat;

cdr_by_user = group cdr_cells_for_bc by caller_msisdn;

result = foreach cdr_by_user {
    ordered = ORDER cdr_cells_for_bc BY call_date;
    bc_metrics = bcudf.calc_indicators(ordered);
    generate bc_metrics;
};
result = foreach result generate flatten(ret);

store result INTO '/user/d4dteam02/bandicoot/bc_metrics/XX' using PigStorage(';'); -- here again substitute XX with month 
