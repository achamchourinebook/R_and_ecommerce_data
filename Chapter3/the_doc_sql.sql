create view vwPromos
as
/*
    retreive a couple of top promotions (see top(2) )
*/
with promos(PromotionKey)
as(
select top(2) PromotionKey
  from (
select f.PromotionKey, p.EnglishPromotionName, count(*) as cnt 
  from FactResellerSales f
       join DimPromotion p on p.PromotionKey = f.PromotionKey
 where f.PromotionKey != 1 
   and p.EnglishPromotionName not like '%volume%'
group by f.PromotionKey, p.EnglishPromotionName 
) t
order by cnt desc
),

PromoSales(PromotionKey, ProductKey, StartDate, EndDate, days, Units, UnitPrice)
as (
select s.PromotionKey, s.ProductKey, 
       min(OrderDate) as StartDate, 
       max(OrderDate) as EndDate, 
       count(distinct OrderDate) as days, 
       sum(OrderQuantity) as Units,
       avg(UnitPrice) as UnitPrice
  from FactResellerSales s
 where s.PromotionKey in (select PromotionKey from promos)
group by s.PromotionKey, s.ProductKey
),

RegularSales(ProductKey, StartDate, EndDate, days, Units, UnitPrice)
as (
select s.ProductKey, 
       min(OrderDate) as StartDate, 
       max(OrderDate) as EndDate, 
       count(distinct OrderDate) as days, 
       sum(OrderQuantity) as Units,
       avg(UnitPrice) as UnitPrice
  from FactResellerSales s
 where (s.PromotionKey = 1 or s.PromotionKey is null)
   and s.ProductKey in (select distinct ProductKey from PromoSales)
group by s.ProductKey
)

select p.PromotionKey,
       dp.ProductKey, dp.EnglishProductName,
       p.Units/cast(p.days as money) as PromoUnitsPerDay,
       p.UnitPrice as PromoUnitPrice,
       r.Units/cast(r.days as money) as RegUnitsPerDay,
       r.UnitPrice as RegUnitPrice
  from PromoSales p
       join RegularSales r on r.ProductKey = p.ProductKey
       join DimProduct dp on dp.ProductKey = p.ProductKey
go
create view [dbo].[vwPromos2]
as
/*
    returns daily sales for promotional and regular sales
*/
with promos(PromotionKey)
as(
select top(2) PromotionKey
  from (
select f.PromotionKey, p.EnglishPromotionName, count(*) as cnt 
  from FactResellerSales f
       join DimPromotion p on p.PromotionKey = f.PromotionKey
 where f.PromotionKey != 1 
   and p.EnglishPromotionName not like '%volume%'
group by f.PromotionKey, p.EnglishPromotionName 
) t
order by cnt desc
),

PromoSales(PromotionKey, ProductKey, OrderDate, Units)
as (
select s.PromotionKey, s.ProductKey,
       cast(OrderDate as date) as OrderDate, 
       sum(OrderQuantity) as Units
  from FactResellerSales s
 where s.PromotionKey in (select PromotionKey from promos)
group by s.PromotionKey, s.ProductKey, cast(OrderDate as date)
),

RegularSales(ProductKey, OrderDate, Units)
as (
select s.ProductKey, 
       cast(OrderDate as date) as OrderDate, 
       sum(OrderQuantity) as Units
  from FactResellerSales s
 where (s.PromotionKey = 1 or s.PromotionKey is null)
   and s.ProductKey in (select distinct ProductKey from PromoSales)
group by s.ProductKey, cast(OrderDate as date)
)

select 1 as PromotionKey, ProductKey, Units from RegularSales
union
select PromotionKey, ProductKey, Units from PromoSales
go
