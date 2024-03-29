
CREATE DATABASES cell_line_schema;
CREATE TABLE Tank(
    Name VARCHAR(100) NOT NULL, #Tank4, NC Tank, Glacier
    PRIMARY KEY(Name)
);
CREATE TABLE Rack(
    Id INT unsigned NOT NULL AUTO_INCREMENT,
    Name INT NOT NULL,
    Tank VARCHAR(100) NOT NULL,
    PRIMARY KEY (Id),
    FOREIGN KEY (Tank) REFERENCES Tank(Name)
);
CREATE TABLE Box(
    Id INT unsigned NOT NULL AUTO_INCREMENT,
    Number INT NOT NULL, 
    Rack_id INT unsigned NOT NULL,
    PRIMARY KEY (Id),
    FOREIGN KEY (Rack_id) REFERENCES Rack(Id)
);
CREATE TABLE Location(
    Id INT unsigned NOT NULL AUTO_INCREMENT,
    Name VARCHAR(5) NOT NULL, #e.g. A1, B9,1,11 ...
    Box_id INT unsigned NOT NULL,
    PRIMARY KEY (Id),
    FOREIGN KEY (Box_id) REFERENCES Box(Id)
);

CREATE TABLE Vial(
    Id INT unsigned NOT NULL AUTO_INCREMENT,
    Location_id INT unsigned NOT NULL, 
    Line VARCHAR(20),
    Passage VARCHAR(5),
    Origin VARCHAR(20),
    Type VARCHAR(20),
    Subtype VARCHAR(20),
    Pool VARCHAR(5),
    CRISPR_EDIT VARCHAR(5),
    Genotype VARCHAR(20),
    Reprogramming_method VARCHAR(20),
    Age INT(2),
    Gender CHAR(1),
    Info VARCHAR(30),
    Media VARCHAR(20),
    ECM VARCHAR(20),
    Date VARCHAR(10),
    Initials VARCHAR(10),
    Cell_numbers VARCHAR(10),
    Confluency INT(2),   
    PRIMARY KEY (Id),
    FOREIGN KEY (Location_id) REFERENCES Location(Id)
);

CREATE TABLE Note(
    Id INT unsigned NOT NULL AUTO_INCREMENT,
    Note VARCHAR(500),
    Vial_id INT unsigned NOT NULL,
    PRIMARY KEY (Id),
    FOREIGN KEY (Vial_id) REFERENCES Vial(Id)
);

