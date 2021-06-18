create view vwStyles
as
select id, replace(masterCategory,' ','_')+'-'+replace(subCategory,' ','_') as category 
  from styles
go