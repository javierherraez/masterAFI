SET IDENTITY_INSERT constructor ON;
INSERT INTO constructor (Id, name, nationality)
SELECT DISTINCT constructorId, constructorName, constructorNationality FROM f1_results;
SET IDENTITY_INSERT constructor OFF;

---------------------------------------
SET IDENTITY_INSERT driver ON;
INSERT INTO driver (Id, Ref, name, surname, nationality, number)
SELECT DISTINCT driverId, driverRef, driverName, driverSurname, driverNationality, driverNumber FROM f1_results;
SET IDENTITY_INSERT driver OFF;

---------------------------------------
SELECT DISTINCT driverId, constructorId, season FROM f1_results ORDER BY driverId, constructorId, season;
INSERT INTO driver_drives_for_constructor(driver_id, constructor_id, season)
SELECT DISTINCT driverId, constructorId, season FROM f1_results ORDER BY driverId, constructorId, season;

---------------------------------------
SET IDENTITY_INSERT circuit ON;
INSERT INTO circuit (Id, Ref, Location, Country)
SELECT DISTINCT circuitId, circuitRef, circuitLocation, circuitCountry FROM f1_results;
SET IDENTITY_INSERT circuit OFF;

---------------------------------------
SET IDENTITY_INSERT race ON;
INSERT INTO race (Id, season, name, round, date, circuit_id)
SELECT DISTINCT raceId, season, raceName, raceRound, raceDate, circuitId  FROM f1_results;
SET IDENTITY_INSERT race OFF;

---------------------------------------
SET IDENTITY_INSERT result ON;
INSERT INTO result (Id, grid_quali, positionText, positionOrder, points, laps, milliseconds, fastestLap, fastestLapSpeed, fastestLapTime, status_id, status, driver_id, race_id)
SELECT DISTINCT resultId, gridQuali, positionText, positionOrder, points, laps, milliseconds, fastestLap, fastestLapSpeed, fastestLapTime, statusId, status, driverId, raceId FROM f1_results;
SET IDENTITY_INSERT result OFF;