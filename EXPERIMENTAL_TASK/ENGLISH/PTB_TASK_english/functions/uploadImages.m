function [var] = uploadImages (var)

% last modifed on August 2017 by Eva

switch var.country
    
    case 1 % California
        
        % SWEET OPTION
        switch var.sweet
            
            case 1  % case participant likes M&Ms the most
                [var.sweetImage, ~, alpha] = imread('images/MM.png');
                var.sweetImage(:,:,4) = alpha;
                var.sweetLabel = 'M&Ms';
                
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
                var.saltyLabel = 'Cashews';
                
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
                var.sweetLabel = 'M&Ms';
                
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
                var.saltyLabel = 'Cashews';
                
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
        
        
end

