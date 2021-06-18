create view [dbo].[vwSalesByState]
as
select t.StateProvinceCode, sum(t.sales) as sales
  from (
select g.CountryRegionCode, g.StateProvinceCode, s.SalesAmount as sales
  from FactInternetSales s
       join DimCustomer c on c.CustomerKey = s.CustomerKey
       join DimGeography g on g.GeographyKey = c.GeographyKey
union
select g.CountryRegionCode, g.StateProvinceCode, s.SalesAmount as sales
  from FactResellerSales s
       join DimReseller r on r.ResellerKey = s.ResellerKey
       join DimGeography g on g.GeographyKey = r.GeographyKey) t
 where t.CountryRegionCode = 'US'
group by t.StateProvinceCode
go
create view [dbo].[vwSalesByStateYear]
as
select t.StateProvinceCode, t.year, sum(t.sales) as sales
  from (
select g.CountryRegionCode, g.StateProvinceCode, year(s.OrderDate) as year, s.SalesAmount as sales
  from FactInternetSales s
       join DimCustomer c on c.CustomerKey = s.CustomerKey
       join DimGeography g on g.GeographyKey = c.GeographyKey
union
select g.CountryRegionCode, g.StateProvinceCode, year(s.OrderDate) as year, s.SalesAmount as sales
  from FactResellerSales s
       join DimReseller r on r.ResellerKey = s.ResellerKey
       join DimGeography g on g.GeographyKey = r.GeographyKey) t
 where t.CountryRegionCode = 'US'
group by t.StateProvinceCode, t.year
go
create view vwSalesByStateAndProduct
as
select customer_state as StateProvinceCode, p.product_category_name_en as product, sales = sum(i.price)
  from olist_order_items as i
       inner join olist_products as p on p.product_id = i.product_id
       inner join olist_orders o on o.order_id = i.order_id
       inner join olist_customers c on c.customer_id = o.customer_id
group by customer_state, p.product_category_name_en
go