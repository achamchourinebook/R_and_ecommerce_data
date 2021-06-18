create view vwOrdersByDay
as

select cast(order_date as date) as [date], count(*) as orders, sum(units) as units, sum(sales) as sales 
  from (
select o.order_id, min(o.order_purchase_timestamp) as order_date, count(*) as units, sum(i.price) as sales  
  from olist_order_items i
       join olist_orders o on o.order_id = i.order_id
group by o.order_id) t
group by cast(order_date as date)
go