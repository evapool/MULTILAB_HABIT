function [var] = uploadImages (var)

% last modifed on June 2016 

snackTypeText = importdata('instructions/snackTypes.txt');

% SWEET OPTION    
switch var.sweet
    
    case 1  % case participant likes M&Ms the most
        [var.sweetImage, ~, alpha] = imread('images/MM.png');
        var.sweetImage(:,:,4) = alpha;
        var.sweetLabel = snackTypeText{1};
        
    case 2 % case participant likes Riesen the most
        [var.sweetImage, ~, alpha] = imread('images/Riesen.png');
        var.sweetImage(:,:,4) = alpha;
        var.sweetLabel = snackTypeText{2};

    case 3  % case participant likes Schokobons the most
        [var.sweetImage, ~, alpha] = imread('images/Schokobon.png');
        var.sweetImage(:,:,4) = alpha;
        var.sweetLabel = snackTypeText{3};
        
    case 4 % case participant likes Erdnüsse the most 
        [var.saltyImage, ~, alpha] = imread('images/Erdnüsse.png');
        var.saltyImage(:,:,4) = alpha;
        var.saltyLabel = snackTypeText{4};
        
    case 5 % case participant likes Lays Chips the most 
        [var.saltyImage, ~, alpha] = imread('images/Chips.png');
        var.saltyImage(:,:,4) = alpha;
        var.saltyLabel = snackTypeText{5};
        
    case 6 % case participant likes TUC cracker the most
        [var.saltyImage, ~, alpha] = imread('images/TUC.png');
        var.saltyImage(:,:,4) = alpha;
        var.saltyLabel = snackTypeText{6};
        
end



% SALTY OPTION
switch var.salty
             
        case 1  % case participant likes M&Ms the most
        [var.sweetImage, ~, alpha] = imread('images/MM.png');
        var.sweetImage(:,:,4) = alpha; % set background of the png as transparent
        var.sweetLabel = snackTypeText{1};
        
    case 2 % case participant likes chocolate rise the most
        [var.sweetImage, ~, alpha] = imread('images/Riesen.png');
        var.sweetImage(:,:,4) = alpha;
        var.sweetLabel = snackTypeText{2};

    case 3  % case participant likes skittles the most
        [var.sweetImage, ~, alpha] = imread('images/Schokobon.png');
        var.sweetImage(:,:,4) = alpha;
        var.sweetLabel = snackTypeText{3};
        
    case 4 % case participant likes  cashew the most 
        [var.saltyImage, ~, alpha] = imread('images/Erdnüsse.png');
        var.saltyImage(:,:,4) = alpha;
        var.saltyLabel = snackTypeText{4};
        
    case 5 % case participant likes goldenfish craker the most 
        [var.saltyImage, ~, alpha] = imread('images/Chips.png');
        var.saltyImage(:,:,4) = alpha;
        var.saltyLabel = snackTypeText{5};
        
    case 6 % case participant likes  cheez it the most
        [var.saltyImage, ~, alpha] = imread('images/TUC.png');
        var.saltyImage(:,:,4) = alpha;
        var.saltyLabel = snackTypeText{6};
        
end
        
        
end

