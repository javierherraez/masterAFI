create database Formula1
go

use Formula1
go

/*==============================================================*/
/* Table: Circuit                                               */
/*==============================================================*/
create table circuit (
   Id                   int                  identity,
   Ref					nvarchar(40)         not null,
   Location             nvarchar(40)         null,
   Country              nvarchar(40)         not null,
   constraint PK_CIRCUIT primary key (Id)
)
go


/*==============================================================*/
/* Table: "Race"                                                */
/*==============================================================*/
create table race (
   Id                   int                  identity,
   season	            int					 not null,
   name					nvarchar(40)         not null,
   round				int					 null,
   date					datetime			 null,
   circuit_id			int					 not null,
   constraint PK_RACE primary key (Id),
   constraint FK_RACE_CIRCUIT foreign key (circuit_id) references circuit (Id)
)
go


/*==============================================================*/
/* Table: Constructor                                           */
/*==============================================================*/
create table constructor (
   Id                   int                  identity,
   name					nvarchar(40)         not null,
   nationality          nvarchar(40)         null,
   constraint PK_CONSTRUCTOR primary key (Id)
)
go

/*==============================================================*/
/* Table: Driver                                                */
/*==============================================================*/
create table driver (
   Id                   int                  identity,
   Ref					nvarchar(40)         not null,
   name					nvarchar(40)         not null,
   surname				nvarchar(40)         not null,
   number				int                  null,
   nationality			nvarchar(40)         null,
   constraint PK_DRIVER primary key (Id)
)
go

/*==============================================================*/
/* Table: Driver_drives_for_Constructor                         */
/*==============================================================*/
create table driver_drives_for_constructor (
   driver_id            int                  not null,
   constructor_id       int                  not null,
   season				int                  not null,
   constraint PK_DRIVER_DRIVES_FOR_CONSTRUCTOR primary key (driver_id, constructor_id, season),
   constraint FK_DRIVER_DRIVES_FOR_CONSTRUCTOR_DRIVER foreign key (driver_id) references driver (Id),
   constraint FK_DRIVER_DRIVES_FOR_CONSTRUCTOR_CONSTRUCTOR foreign key (constructor_id) references constructor (Id)
)
go


/*==============================================================*/
/* Table: Result                                              */
/*==============================================================*/
create table result (
   Id                   int                  identity,
   grid_quali           int					 null,
   positionText         nvarchar(40)         null,
   positionOrder        int				     null,
   points               float                null,
   laps                 int				     null,
   milliseconds         int				     null,
   fastestLap           int				     null,
   fastestLapSpeed      float                null,
   fastestLapTime       float                null,
   status_id             int					 not null,
   status               nvarchar(40)		 null,
   driver_id			int					 not null,
   race_id				int					 not null
   constraint PK_RESULT primary key (Id),
   constraint FK_RESULT_DRIVER foreign key (driver_id) references driver (Id),
   constraint FK_RESULT_RACE foreign key (race_id) references race (Id)
)
go
