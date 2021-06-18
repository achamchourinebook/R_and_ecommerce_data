create view [dbo].[vwOrdersByDay_with_blanks]
as
/* 
   adding 365 extra days using system table with enough number of records
   any other suitable object (view,table) can be used 

   trimming leading and trailing data points in question
*/

select cast(order_date as date) as [date], count(*) as orders, sum(units) as units, sum(sales) as sales
  from (
select o.order_id, min(o.order_purchase_timestamp) as order_date, count(*) as units, sum(i.price) as sales  
  from olist_order_items i
       join olist_orders o on o.order_id = i.order_id
 where order_purchase_timestamp >= '2017-01-05' and order_purchase_timestamp < '2018-08-20'
group by o.order_id) t
group by cast(order_date as date)
union 
select top(365) dateadd(dd, row_number() over(order by name), md), null, null, null
  from master.dbo.spt_values t
       join (select max(cast(order_purchase_timestamp as date)) as md 
			   from olist_orders
		      where order_purchase_timestamp >= '2017-01-05' and order_purchase_timestamp < '2018-08-20'
) x on 1=1
go