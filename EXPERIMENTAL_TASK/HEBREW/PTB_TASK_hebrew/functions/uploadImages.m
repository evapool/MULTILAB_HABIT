function [var] = uploadImages (var)

% last modifed on August 2017 by Eva

switch var.country
    
    case 1 % California
        
        % SWEET OPTION
        switch var.sweet
            
            case 1  % case participant likes M&Ms the most
                [var.sweetImage, ~, alpha] = imread('images/MM.png');
                var.sweetImage(:,:,4) = alpha;
                var.sweetLabel = char([1513 1493 1511 1493 1500 1491]);
                
            case 2 % case participant likes chocolate rise the most
                [var.sweetImage, ~, alpha] = imread('images/chocorise.png');
                var.sweetImage(:,:,4) = alpha;
                var.sweetLabel = 'Choco Rice';
                
            case 3  % case participant likes skittles the most
                [var.sweetImage, ~, alpha] = imread('images/skittles.png');
                var.sweetImage(:,:,4) = alpha;
                var.sweetLabel = 'Skittles';
                
            case 4 % case participant likes  cashew the most
                [var.saltyImage, ~, alpha] = imread('images/cashew.png');
                var.saltyImage(:,:,4) = alpha;
                var.saltyLabel = char([1489 1497 1497 1490 1500 1492]);
                
            case 5 % case participant likes goldfish cracker the most
                [var.saltyImage, ~, alpha] = imread('images/goldenfish.png');
                var.saltyImage(:,:,4) = alpha;
                var.saltyLabel = 'Goldfish crackers';
                
            case 6 % case participant likes cheez it the most
                [var.saltyImage, ~, alpha] = imread('images/cheezit.png');
                var.saltyImage(:,:,4) = alpha;
                var.saltyLabel = 'Cheez-it crackers';
                
        end
        
        
        
        % SALTY OPTION
        switch var.salty
            
            case 1  % case participant likes M&Ms the most
                [var.sweetImage, ~, alpha] = imread('images/MM.png');
                var.sweetImage(:,:,4) = alpha; % set background of the png as transparent
                var.sweetLabel = char([1513 1493 1511 1493 1500 1491]);
                
            case 2 % case participant likes chocolate rice the most
                [var.sweetImage, ~, alpha] = imread('images/chocorise.png');
                var.sweetImage(:,:,4) = alpha;
                var.sweetLabel = 'Choco Rice';
                
            case 3  % case participant likes skittles the most
                [var.sweetImage, ~, alpha] = imread('images/skittles.png');
                var.sweetImage(:,:,4) = alpha;
                var.sweetLabel = 'Skittles';
                
            case 4 % case participant likes  cashew the most
                [var.saltyImage, ~, alpha] = imread('images/cashew.png');
                var.saltyImage(:,:,4) = alpha;
                var.saltyLabel = char([1489 1497 1497 1490 1500 1492]);
                
            case 5 % case participant likes goldfish craker the most
                [var.saltyImage, ~, alpha] = imread('images/goldenfish.png');
                var.saltyImage(:,:,4) = alpha;
                var.saltyLabel = 'Goldfish crackers';
                
            case 6 % case participant likes  cheez it the most
                [var.saltyImage, ~, alpha] = imread('images/cheezit.png');
                var.saltyImage(:,:,4) = alpha;
                var.saltyLabel = 'Cheez-it crackers';
                
        end
        
        
    case 2 % Australia
        
        % SWEET OPTION
        switch var.sweet
            
            case 1  % case participant likes M&Ms the most
                [var.sweetImage, ~, alpha] = imread('images/MM.png');
                var.sweetImage(:,:,4) = alpha;
                var.sweetLabel = 'M&Ms';
                
            case 2 % case participant likes malters the most
                [var.sweetImage, ~, alpha] = imread('images/maltesers.png');
                var.sweetImage(:,:,4) = alpha;
                var.sweetLabel = 'Maltesers';
                
            case 3  % case participant likes skittles the most
                [var.sweetImage, ~, alpha] = imread('images/skittles.png');
                var.sweetImage(:,:,4) = alpha;
                var.sweetLabel = 'Skittles';
                
            case 4 % case participant likes  cashews the most
                [var.saltyImage, ~, alpha] = imread('images/cashew.png');
                var.saltyImage(:,:,4) = alpha;
                var.saltyLabel = 'Cashews';
                
            case 5 % case participant likes doritos the most
                [var.saltyImage, ~, alpha] = imread('images/doritos.png');
                var.saltyImage(:,:,4) = alpha;
                var.saltyLabel = 'Doritos';
                
            case 6 % case participant likes chips it the most
                [var.saltyImage, ~, alpha] = imread('images/chip.png');
                var.saltyImage(:,:,4) = alpha;
                var.saltyLabel = 'Chips';
                
        end
        
        
        
        % SALTY OPTION
        switch var.salty
            
            case 1  % case participant likes M&Ms the most
                [var.sweetImage, ~, alpha] = imread('images/MM.png');
                var.sweetImage(:,:,4) = alpha; % set background of the png as transparent
                var.sweetLabel = 'M&Ms';
                
            case 2 % case participant likes malters the most
                [var.sweetImage, ~, alpha] = imread('images/maltesers.png');
                var.sweetImage(:,:,4) = alpha;
                var.sweetLabel = 'Maltesers';
                
            case 3  % case participant likes skittles the most
                [var.sweetImage, ~, alpha] = imread('images/skittles.png');
                var.sweetImage(:,:,4) = alpha;
                var.sweetLabel = 'Skittles';
                
            case 4 % case participant likes  cashew the most
                [var.saltyImage, ~, alpha] = imread('images/cashew.png');
                var.saltyImage(:,:,4) = alpha;
                var.saltyLabel = 'Cashews';
                
            case 5 % case participant likes doritos the most
                [var.saltyImage, ~, alpha] = imread('images/doritos.png');
                var.saltyImage(:,:,4) = alpha;
                var.saltyLabel = 'Doritos';
                
            case 6 % case participant likes  chipit the most
                [var.saltyImage, ~, alpha] = imread('images/chip.png');
                var.saltyImage(:,:,4) = alpha;
                var.saltyLabel = 'Chips';
                
        end
        
        %---Rani
    case 3 %ISRAEL
        
        % SWEET OPTION
        switch var.sweet
            
            case 1  % case participant likes M&Ms the most
                [var.sweetImage, ~, alpha] = imread('images/MM.png');
                var.sweetImage(:,:,4) = alpha;
                var.sweetLabel = 'M&Ms';
                var.sweetLabelHebrew = [77 38 77];
                
            case 2 % case participant likes malters the most
                [var.sweetImage, ~, alpha] = imread('images/Click.png');
                var.sweetImage(:,:,4) = alpha;
                var.sweetLabel = 'Click';
                var.sweetLabelHebrew = [1511 1500 1497 1511];
                
            case 3  % case participant likes skittles the most
                [var.sweetImage, ~, alpha] = imread('images/skittles.png');
                var.sweetImage(:,:,4) = alpha;
                var.sweetLabel = 'Skittles';
                var.sweetLabelHebrew = [1505 1511 1497 1496 1500 1505];
                var.saltyLabelHebrew = [];
                
            case 4 % case participant likes  cashews the most
                [var.saltyImage, ~, alpha] = imread('images/cashew.png');
                var.saltyImage(:,:,4) = alpha;
                var.saltyLabel = 'Cashews';
                var.saltyLabelHebrew = [1511 1513 1497 1493];

            case 5 % case participant likes doritos the most
                [var.saltyImage, ~, alpha] = imread('images/doritos.png');
                var.saltyImage(:,:,4) = alpha;
                var.saltyLabel = 'Doritos';
                var.saltyLabelHebrew = [1491 1493 1512 1497 1496 1493 1505];
                
            case 6 % case participant likes chips it the most
                [var.saltyImage, ~, alpha] = imread('images/TapuChips.png');
                var.saltyImage(:,:,4) = alpha;
                var.saltyLabel = 'Tapuchips';
                var.saltyLabelHebrew = [1514 1508 1493 1510 39 1497 1508 1505];
                
        end
        
        
        
        % SALTY OPTION
        switch var.salty
            
            case 1  % case participant likes M&Ms the most
                [var.sweetImage, ~, alpha] = imread('images/MM.png');
                var.sweetImage(:,:,4) = alpha; % set background of the png as transparent
                var.sweetLabel = 'M&Ms';
                var.sweetLabelHebrew = [77 38 77];
                
            case 2 % case participant likes malters the most
                [var.sweetImage, ~, alpha] = imread('images/Click.png');
                var.sweetImage(:,:,4) = alpha;
                var.sweetLabel = 'Click';
                var.sweetLabelHebrew = [1511 1500 1497 1511];
                
            case 3  % case participant likes skittles the most
                [var.sweetImage, ~, alpha] = imread('images/skittles.png');
                var.sweetImage(:,:,4) = alpha;
                var.sweetLabel = 'Skittles';
                var.sweetLabelHebrew = [1505 1511 1497 1496 1500 1505];
                
            case 4 % case participant likes  cashew the most
                [var.saltyImage, ~, alpha] = imread('images/cashew.png');
                var.saltyImage(:,:,4) = alpha;
                var.saltyLabel = 'Cashews';
                var.saltyLabelHebrew = [1511 1513 1497 1493];
                
            case 5 % case participant likes doritos the most
                [var.saltyImage, ~, alpha] = imread('images/doritos.png');
                var.saltyImage(:,:,4) = alpha;
                var.saltyLabel = 'Doritos';
                var.saltyLabelHebrew = [1491 1493 1512 1497 1496 1493 1505];
                
            case 6 % case participant likes  chipit the most
                [var.saltyImage, ~, alpha] = imread('images/TapuChips.png');
                var.saltyImage(:,:,4) = alpha;
                var.saltyLabel = 'Tapuchips';
                var.saltyLabelHebrew = [1514 1508 1493 1510 39 1497 1508 1505];
                
        end
        %---
        
end

