create view vwProductNames
as

select distinct EnglishProductName as product from DimProduct
go