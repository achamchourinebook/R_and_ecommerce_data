create view [dbo].[vwDiscount]
as

select t.product_id, 
       discount=(avg(price)-min(price))/avg(price) *100
  from (
select i.product_id, i.price, cnt=count(*)
  from olist_order_items i
       join olist_orders o on o.order_id = i.order_id
group by i.product_id, i.price) t
group by t.product_id
having count(*) > 1 
and (avg(price)-min(price))/avg(price) >= 0.01
go
create view vwProducts
as
select t.product_id as id, 
       avg(t.DaysToManufacture) as DaysToManufacture, 
       avg(t.DaysToDeliver) as DaysToDeliver,
       min(t.OrderDate) as StartDate,
       cast(sum(t.units) as money)/(datediff(dd, min(t.OrderDate), '2018-09-03')+1) as UnitsNorm,
       avg(t.price) as Price,
       min(p.scategory) as Category,
       min(p.product_description_lenght) as DescLength,
       min(p.product_photos_qty) as PhotosQty,
       min(product_weight_g) as PWeight,
       min(product_length_cm) as PLength
  from (
select i.product_id, 
       DaysToManufacture = datediff(dd, o.order_purchase_timestamp, o.order_delivered_carrier_date),
       DaysToDeliver = datediff(dd, o.order_delivered_carrier_date, o.order_delivered_customer_date),
       OrderDate = cast(o.order_purchase_timestamp as date),
       units = 1,
       i.order_id,
       i.price
  from olist_orders o
       join olist_order_items i on i.order_id = o.order_id
 where o.order_delivered_carrier_date != '1900-01-01'
   and o.order_delivered_customer_date != '1900-01-01'
   and o.order_status != 'canceled'
      ) t
        join olist_products p on p.product_id = t.product_id
 where t.DaysToManufacture >= 0
   and t.DaysToDeliver >= 0
group by t.product_id
having count(distinct t.order_id) >= 10
go
create view vwProducts_cats
as
with cats(Category)
as (
select Category
  from vwProducts p
group by Category
having count(*) >= 10
)

select p.*, 
       isnull(d.discount,0) as [DiscPrcnt]
  from vwProducts p
       join cats c on c.Category = p.Category
       left join vwDiscount d on d.product_id = p.id
go