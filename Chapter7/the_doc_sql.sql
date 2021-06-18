create view vwMBA
as

select t.tid, p.EnglishProductName as product
  from (select distinct SalesOrderNumber as tid, s.ProductKey
          from FactInternetSales s
       ) t
       join DimProduct p on p.ProductKey = t.ProductKey
go