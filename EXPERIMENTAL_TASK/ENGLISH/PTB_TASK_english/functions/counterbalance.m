function [var, data] = counterbalance(var)

% Last modifed on May 2017 by Eva
% Counterbalance the action-image-response association based on the participant ID
% the counterbalancement is complete only if there the same two actions
% possibles takes only the first three fractals from the original task 


if var.sub_ID == 1 || mod((var.sub_ID - 1),12) == 0;
    data.list = 1;
    
    var.sweet_fractal        = imread('images/fractal1.jpg');
    var.sweet_action         = var.leftKey;
    var.sweet_square_pos     = var.square1; % square to the left

    var.salty_fractal        = imread('images/fractal2.jpg');
    var.salty_action         = var.centerLeftKey;
    var.salty_square_pos     = var.square2;% square to the right
    
    var.rest_fractal         = imread('images/fractal3.jpg');
      
elseif var.sub_ID == 2 || mod((var.sub_ID - 2),12) == 0;
    data.list = 2;
    
    var.sweet_fractal        = imread('images/fractal1.jpg');
    var.sweet_action         = var.centerLeftKey;
    var.sweet_square_pos     = var.square2; 

    var.salty_fractal        = imread('images/fractal2.jpg');
    var.salty_action         = var.leftKey;
    var.salty_square_pos     = var.square1;

    var.rest_fractal          = imread('images/fractal3.jpg');
    
elseif var.sub_ID == 3 || mod((var.sub_ID - 3),12) == 0;
    data.list = 3;
    
    var.sweet_fractal        = imread('images/fractal2.jpg');
    var.sweet_action         = var.leftKey;
    var.sweet_square_pos     = var.square1;

    var.salty_fractal        = imread('images/fractal1.jpg');
    var.salty_action         = var.centerLeftKey;
    var.salty_square_pos     = var.square2;
    
    var.rest_fractal         = imread('images/fractal3.jpg'); 
       
elseif var.sub_ID == 4 || mod((var.sub_ID - 4),12) == 0;
    data.list = 4;
    
    var.sweet_fractal        = imread('images/fractal2.jpg');
    var.sweet_action         = var.centerLeftKey;
    var.sweet_square_pos     = var.square2;
    
    var.salty_fractal        = imread('images/fractal1.jpg');
    var.salty_action         = var.leftKey;
    var.salty_square_pos     = var.square1;
    
    var.rest_fractal         = imread('images/fractal3.jpg');
        
elseif var.sub_ID == 5 || mod((var.sub_ID - 5),12) == 0;
    data.list = 5;
    
    var.sweet_fractal        = imread('images/fractal3.jpg');
    var.sweet_action         = var.leftKey;
    var.sweet_square_pos     = var.square1;
    
    var.salty_fractal        = imread('images/fractal1.jpg');
    var.salty_action         = var.centerLeftKey;
    var.salty_square_pos     = var.square2;
    
    var.rest_fractal         = imread('images/fractal2.jpg');
    
elseif var.sub_ID == 6 || mod((var.sub_ID - 6),12) == 0;
    data.list = 6;
    
    var.sweet_fractal        = imread('images/fractal3.jpg');
    var.sweet_action         = var.centerLeftKey;
    var.sweet_square_pos     = var.square2;
    
    var.salty_fractal        = imread('images/fractal1.jpg');
    var.salty_action         = var.leftKey;
    var.salty_square_pos     = var.square1;
    
    var.rest_fractal        = imread('images/fractal2.jpg');
        
elseif var.sub_ID == 7 || mod((var.sub_ID - 7),12) == 0;
    data.list = 7;
    
    var.sweet_fractal        = imread('images/fractal1.jpg');
    var.sweet_action         = var.centerLeftKey;
    var.sweet_square_pos     = var.square2;
    
    var.salty_fractal        = imread('images/fractal3.jpg');
    var.salty_action         = var.leftKey; 
    var.salty_square_pos     = var.square1;
    
    var.rest_fractal         = imread('images/fractal2.jpg');
        
elseif var.sub_ID == 8 || mod((var.sub_ID - 8),12) == 0;
    data.list = 8;
    
    var.sweet_fractal        = imread('images/fractal1.jpg');
    var.sweet_action         = var.leftKey;
    var.sweet_square_pos     = var.square1;
    
    var.salty_fractal        = imread('images/fractal3.jpg');
    var.salty_action         = var.centerLeftKey;
    var.salty_square_pos     = var.square2;

    var.rest_fractal         = imread('images/fractal2.jpg');
    
elseif var.sub_ID == 9 || mod((var.sub_ID - 9),12) == 0;
    data.list = 9;
    
    var.sweet_fractal        = imread('images/fractal2.jpg');
    var.sweet_action         = var.centerLeftKey;
    var.sweet_square_pos     = var.square2;
    
    var.salty_fractal        = imread('images/fractal3.jpg');
    var.salty_action         = var.leftKey;
    var.salty_square_pos     = var.square1;
    
    var.rest_fractal         = imread('images/fractal1.jpg');
    
elseif var.sub_ID == 10 || mod((var.sub_ID - 10),12) == 0;
    data.list = 10;
    
    var.sweet_fractal        = imread('images/fractal2.jpg');
    var.sweet_action         = var.leftKey;
    var.sweet_square_pos     = var.square1;
    
    var.salty_fractal        = imread('images/fractal3.jpg');
    var.salty_action         = var.centerLeftKey;
    var.salty_square_pos     = var.square2;
    
    var.rest_fractal         = imread('images/fractal1.jpg');
       
elseif var.sub_ID == 11 || mod((var.sub_ID - 11),12) == 0;
    data.list = 11;
    
    var.sweet_fractal        = imread('images/fractal3.jpg');
    var.sweet_action         = var.centerLeftKey;
    var.sweet_square_pos     = var.square2;
     
    var.salty_fractal        = imread('images/fractal2.jpg');
    var.salty_action         = var.leftKey;
    var.salty_square_pos     = var.square1;
    
    var.rest_fractal         = imread('images/fractal1.jpg');
    
elseif var.sub_ID == 12 || mod((var.sub_ID - 12),12) == 0;
    data.list = 12;
    
    var.sweet_fractal        = imread('images/fractal3.jpg');
    var.sweet_action         = var.leftKey;
    var.sweet_square_pos     = var.square1;
    
    var.salty_fractal        = imread('images/fractal2.jpg');
    var.salty_action         = var.centerLeftKey;
    var.salty_square_pos     = var.square2;
    
    var.rest_fractal         = imread('images/fractal1.jpg');
    
end


% assign devalued role to one of the snacks (alternate)
if  mod((var.sub_ID-2),2) == 0
    
    var.devalued = 1; % devalue sweet for even numbers
    data.target  = 'sweet';
   
else
    
    var.devalued = 2; % devalue salty for odd numbers
    data.target  = 'salty';

end