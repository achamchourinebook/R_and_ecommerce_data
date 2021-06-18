create view vwPrice
as
select p.sku, i.price, count(*) as cnt
  from olist_order_items as i
       inner join olist_products as p on p.product_id = i.product_id
group by p.sku, i.price
go
