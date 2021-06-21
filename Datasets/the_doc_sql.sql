-- add PKs
alter table olist_customers alter column customer_id varchar(40) not null
alter table olist_customers add primary key(customer_id)

alter table olist_order_items alter column order_id varchar(40) not null 
alter table olist_order_items alter column order_item_id int not null 
alter table olist_order_items add primary key(order_id, order_item_id)

alter table olist_orders alter column order_id varchar(40) not null 
alter table olist_orders add primary key(order_id)

alter table olist_products alter column product_id varchar(40) not null 
alter table olist_products add primary key(product_id)

--- add FKs
alter table olist_order_items add constraint FK_olist_order_items_products foreign key(product_id) references olist_products(product_id)
alter table olist_order_items add constraint FK_olist_order_items_orders foreign key(order_id) references olist_orders(order_id)
alter table olist_orders add constraint FK_olist_orders_customers foreign key(customer_id) references olist_customers(customer_id)

--- new
alter table olist_products add sku varchar(100) null
alter table olist_products add product_category_name_en varchar(100) null
alter table olist_products add scategory varchar(50) null

-- add category
update p
   set product_category_name_en = t.product_category_name_english
  from product_category_name_translation t
       join olist_products p on p.product_category_name = t.product_category_name

update olist_products set product_category_name = 'UNKNOWN' where product_category_name is null or product_category_name = '';

-- add sku
with sku(product_id, sku)
as (
select t.product_id, sku = t.product_category_name_en +'_' +cast(t.rownum as varchar(20))
  from (
select product_id, product_category_name_en, rownum = row_number() over(partition by product_category_name order by product_id) 
  from olist_products) t
) 

update p
   set sku = t.sku
  from olist_products p
       join sku t on t.product_id = p.product_id

-- add super category, this is MS SQL specific syntax 

declare @scategories table(category varchar(50), scategory varchar(50))

insert @scategories values
('agro_industry_and_commerce','tools, industry'), ('air_conditioning','tools, industry'),
('art','art'), ('arts_and_craftmanship','art'), ('audio','comps, electronics'), 
('auto','tools, industry'), ('baby','health, beauty'), ('bed_bath_table','furniture'),
('books_general_interest','books'), ('books_imported','books'),
('books_technical','books'), ('cds_dvds_musicals','leisure'),
('christmas_supplies','leisure'), ('cine_photo','leisure'),
('computers','comps, electronics'), ('computers_accessories','comps, electronics'),
('consoles_games','leisure'), ('construction_tools_construction','tools, industry'),
('construction_tools_lights','tools, industry'), ('construction_tools_safety','tools, industry'),
('cool_stuff','fashion'), ('costruction_tools_garden','tools, industry'),
('costruction_tools_tools','tools, industry'), ('diapers_and_hygiene','health, beauty'),
('drinks','food'), ('dvds_blu_ray','leisure'),
('electronics','comps, electronics'), ('fashio_female_clothing','fashion'),
('fashion_bags_accessories','fashion'), ('fashion_childrens_clothes','fashion'),
('fashion_male_clothing','fashion'), ('fashion_shoes','fashion'),
('fashion_sport','fashion'), ('fashion_underwear_beach','fashion'),
('fixed_telephony','comps, electronics'), ('flowers','leisure'),
('food','food'), ('food_drink','food'), ('furniture_bedroom','furniture'), 
('furniture_decor','furniture'), ('furniture_living_room','furniture'), 
('furniture_mattress_and_upholstery','furniture'), ('garden_tools','tools, industry'), 
('health_beauty','health, beauty'), ('home_appliances','home'), ('home_appliances_2','home'),
('home_comfort_2','home'), ('home_confort','home'),
('home_construction','tools, industry'), ('housewares','home'),
('industry_commerce_and_business','tools, industry'), 
('kitchen_dining_laundry_garden_furniture','furniture'),
('la_cuisine','food'), ('luggage_accessories','fashion'), ('market_place','food'), 
('music','leisure'), ('musical_instruments','leisure'), ('office_furniture','furniture'),
('party_supplies','leisure'), ('perfumery','health, beauty'), ('pet_shop','leisure'), 
('security_and_services','comps, electronics'),
('signaling_and_security','comps, electronics'), ('small_appliances','home'),
('small_appliances_home_oven_and_coffee','home'), ('sports_leisure','leisure'),
('stationery','stationery'), ('tablets_printing_image','comps, electronics'),
('telephony','comps, electronics'), ('toys','leisure'), ('watches_gifts','gifts')

update p set scategory = s.scategory
  from olist_products p
       join product_category_name_translation c on c.product_category_name = p.product_category_name
       join scategories s on s.category = c.product_category_name_english

--- random samples MS SQL specific syntax
declare @t table(CustomerKey int not null)
declare @CustomerKey int
declare @mid bigint, @rnd bigint, @i int = 1, @n int = 10

select @mid = max(Customerkey) from dimCustomer

while (select count(*) from @t) < @n
begin
    set @rnd = floor(rand() *@mid)

    select @CustomerKey = CustomerKey from DimCustomer where CustomerKey = @rnd
    if(@@rowcount > 0) and not exists(select * from @t where CustomerKey = @CustomerKey)
        insert @t(CustomerKey) values(@CustomerKey)

    set @i = @i+1
end

select * from @t

